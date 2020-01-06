;;;; test/hand.lisp
;;;;
;;;; Copyright 2012-2019 Kimmo "keko" Kenttälä and Michał "phoe" Herda.
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a
;;;; copy of this software and associated documentation files (the "Software"),
;;;; to deal in the Software without restriction, including without limitation
;;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;; and/or sell copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included in
;;;; all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;; DEALINGS IN THE SOFTWARE.

(in-package #:riichi-evaluator.test)

(nr:in-readtable :riichi-evaluator)

(defun make-test-hand (&rest args
                       &key
                         (class 'rh:closed-tsumo-hand)
                         (prevailing-wind :east) (seat-wind :east)
                         (winning-tile [1p])
                         (locked-sets '())
                         (free-tiles
                          (rt:read-tile-list-from-string "1112345678999p"))
                         (dora-list '([3p]))
                         (situations '())
                       &allow-other-keys)
  (let* ((known-keys '(:class :prevailing-wind :seat-wind :winning-tile
                       :locked-sets :free-tiles :dora-list :situations))
         (other-keys (apply #'a:remove-from-plist args known-keys)))
    (apply #'make-instance class
           :prevailing-wind prevailing-wind :seat-wind seat-wind
           :winning-tile winning-tile
           :locked-sets locked-sets :free-tiles free-tiles
           :dora-list dora-list :situations situations
           other-keys)))

(define-test hand-negative
  (fail (make-test-hand :prevailing-wind :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :seat-wind :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :winning-tile :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :locked-sets :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :locked-sets '(:keyword)) 'rh:invalid-hand-element)
  (fail (make-test-hand :free-tiles
                        (rt:read-tile-list-from-string "1145678999p")
                        :locked-sets (rs:read-set-from-string "123p"))
      'rh:closed-locked-set)
  (fail (make-test-hand :free-tiles
                        (rt:read-tile-list-from-string "2345678999p")
                        :locked-sets (rs:read-set-from-string "111p"))
      'rh:closed-locked-set)
  (fail (make-test-hand :free-tiles
                        (rt:read-tile-list-from-string "11123456789p")
                        :locked-sets (rs:read-set-from-string "99p"))
      'rh:closed-locked-set)
  (fail (make-test-hand :free-tiles :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :free-tiles '(:keyword)) 'rh:invalid-hand-element)
  (fail (make-test-hand :dora-list :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :dora-list '(:keyword)) 'rh:invalid-hand-element)
  (fail (make-test-hand :dora-list '()) 'rh:invalid-dora-list-length)
  (fail (make-test-hand :dora-list '([4p] [5p] [6p] [4p] [5p] [6p]))
      'rh:invalid-dora-list-length)
  (fail (make-test-hand :ura-dora-list :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :ura-dora-list '(:keyword)) 'rh:invalid-hand-element)
  (fail (make-test-hand :ura-dora-list '([4p] [5p] [6p] [4p] [5p] [6p]))
      'rh:invalid-dora-list-length)
  (fail (make-test-hand :situations :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :situations '(42)) 'rh:invalid-hand-element)
  (fail (make-test-hand :situations '((42))) 'rh:invalid-hand-element)
  (fail (make-test-hand :class 'rh:closed-ron-hand :taken-from :keyword)
      'rh:invalid-hand-element)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "11112345678999p"))
      'rh:invalid-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "1112345678999p")
         :locked-sets (rs:read-set-from-string "4*56p"))
      'rh:invalid-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "112345678999p"))
      'rh:invalid-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "1112345699999p"))
      'rh:invalid-same-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "1112378999p")
         :locked-sets (rs:read-set-from-string "456*p"))
      'rh:minjun-invalid-meld)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "1112378999p")
         :locked-sets (rs:read-set-from-string "456p"))
      'rh:closed-locked-set))

(define-test hand-positive
  (dolist (args '((:class rh:closed-tsumo-hand)
                  (:class rh:closed-ron-hand
                   :taken-from :toimen)
                  (:class rh:open-tsumo-hand)
                  (:class rh:open-ron-hand
                   :taken-from :toimen)))
    (let ((expected-hand
            '([1p] [1p] [1p] [2p] [3p] [4p] [5p]
              [6p] [7p] [8p] [9p] [9p] [9p]))
          (expected-visible-tiles
            '([1p] [1p] [1p] [2p] [3p] [4p] [5p]
              [6p] [7p] [8p] [9p] [9p] [9p]
              [1p] [3p]))
          (hand (apply #'make-test-hand args)))
      (is eq :east (rh:prevailing-wind hand))
      (is eq :east (rh:seat-wind hand))
      (is rt:tile= [1p] (rh:winning-tile hand))
      (is eq '() (rh:locked-sets hand))
      (is rt:tile-list= '([3p]) (rh:dora-list hand))
      (is eq '() (rh:situations hand))
      (is rt:tile-list= expected-hand (rh:free-tiles hand))
      (is rt:tile-list= expected-visible-tiles
          (rh:hand-total-visible-tiles hand))
      (typecase hand
        (rh:closed-tsumo-hand
         (is eq '() (rh:ura-dora-list hand)))
        (rh:closed-ron-hand
         (is eq :toimen (rh:taken-from hand))
         (is eq '() (rh:ura-dora-list hand)))
        (rh:open-tsumo-hand)
        (rh:open-ron-hand
         (is eq :toimen (rh:taken-from hand)))))))

;;; Ordering finder

(defun test-orderings (hand &rest expected-orderings)
  (let ((actual-orderings (rh:find-orderings hand)))
    (dolist (ordering actual-orderings)
      (true (find ordering expected-orderings :test #'rh:ordering=)
            "~S not found in:~%~S" ordering expected-orderings))
    (dolist (ordering expected-orderings)
      (true (find ordering actual-orderings :test #'rh:ordering=)
            "~S not found in:~%~S" ordering expected-orderings))))

;;; Ordering tests - chuuren poutou

(define-test find-orderings-chuuren-poutou-tsumo
  (flet ((do-test (tile &rest orderings)
           (apply #'test-orderings
                  (make-test-hand :winning-tile tile) orderings)))
    (do-test [1p]
      (list (rs:anjun [1p])
            (list (rs:ankou [1p]) (rs:anjun [4p])
                  (rs:anjun [7p]) (rs:antoi [9p])))
      (list (rs:ankou [1p])
            (list (rs:anjun [1p]) (rs:anjun [4p])
                  (rs:anjun [7p]) (rs:antoi [9p]))))
    (do-test [2p]
      (list (rs:antoi [2p])
            (list (rs:ankou [1p]) (rs:anjun [3p])
                  (rs:anjun [6p]) (rs:ankou [9p]))))
    (do-test [3p]
      (list (rs:anjun [1p])
            (list (rs:antoi [1p]) (rs:anjun [3p])
                  (rs:anjun [6p]) (rs:ankou [9p])))
      (list (rs:anjun [3p])
            (list (rs:antoi [1p]) (rs:anjun [1p])
                  (rs:anjun [6p]) (rs:ankou [9p]))))
    (do-test [4p]
      (list (rs:anjun [2p])
            (list (rs:ankou [1p]) (rs:anjun [4p])
                  (rs:anjun [7p]) (rs:antoi [9p])))
      (list (rs:anjun [4p])
            (list (rs:ankou [1p]) (rs:anjun [2p])
                  (rs:anjun [7p]) (rs:antoi [9p]))))
    (do-test [5p]
      (list (rs:antoi [5p])
            (list (rs:ankou [1p]) (rs:anjun [2p])
                  (rs:anjun [6p]) (rs:ankou [9p]))))
    (do-test [6p]
      (list (rs:anjun [4p])
            (list (rs:antoi [1p]) (rs:anjun [1p])
                  (rs:anjun [6p]) (rs:ankou [9p])))
      (list (rs:anjun [6p])
            (list (rs:antoi [1p]) (rs:anjun [1p])
                  (rs:anjun [4p]) (rs:ankou [9p]))))
    (do-test [7p]
      (list (rs:anjun [5p])
            (list (rs:ankou [1p]) (rs:anjun [2p])
                  (rs:anjun [7p]) (rs:antoi [9p])))
      (list (rs:anjun [7p])
            (list (rs:ankou [1p]) (rs:anjun [2p])
                  (rs:anjun [5p]) (rs:antoi [9p]))))
    (do-test [8p]
      (list (rs:antoi [8p])
            (list (rs:ankou [1p]) (rs:anjun [2p])
                  (rs:anjun [5p]) (rs:ankou [9p]))))
    (do-test [9p]
      (list (rs:anjun [7p])
            (list (rs:antoi [1p]) (rs:anjun [1p])
                  (rs:anjun [4p]) (rs:ankou [9p])))
      (list (rs:ankou [9p])
            (list (rs:antoi [1p]) (rs:anjun [1p])
                  (rs:anjun [4p]) (rs:anjun [7p]))))))

(define-test find-orderings-chuuren-poutou-ron
  (do-all-other-players (player)
    (flet ((do-test (tile &rest orderings)
             (apply #'test-orderings
                    (make-test-hand :class 'rh:closed-ron-hand
                                    :winning-tile tile :taken-from player)
                    orderings)))
      (do-test [1p]
        (list (rs:minjun [1p] [1p] player)
              (list (rs:ankou [1p]) (rs:anjun [4p])
                    (rs:anjun [7p]) (rs:antoi [9p])))
        (list (rs:minkou [1p] player)
              (list (rs:anjun [1p]) (rs:anjun [4p])
                    (rs:anjun [7p]) (rs:antoi [9p]))))
      (do-test [2p]
        (list (rs:mintoi [2p] player)
              (list (rs:ankou [1p]) (rs:anjun [3p])
                    (rs:anjun [6p]) (rs:ankou [9p]))))
      (do-test [3p]
        (list (rs:minjun [1p] [3p] player)
              (list (rs:antoi [1p]) (rs:anjun [3p])
                    (rs:anjun [6p]) (rs:ankou [9p])))
        (list (rs:minjun [3p] [3p] player)
              (list (rs:antoi [1p]) (rs:anjun [1p])
                    (rs:anjun [6p]) (rs:ankou [9p]))))
      (do-test [4p]
        (list (rs:minjun [2p] [4p] player)
              (list (rs:ankou [1p]) (rs:anjun [4p])
                    (rs:anjun [7p]) (rs:antoi [9p])))
        (list (rs:minjun [4p] [4p] player)
              (list (rs:ankou [1p]) (rs:anjun [2p])
                    (rs:anjun [7p]) (rs:antoi [9p]))))
      (do-test [5p]
        (list (rs:mintoi [5p] player)
              (list (rs:ankou [1p]) (rs:anjun [2p])
                    (rs:anjun [6p]) (rs:ankou [9p]))))
      (do-test [6p]
        (list (rs:minjun [4p] [6p] player)
              (list (rs:antoi [1p]) (rs:anjun [1p])
                    (rs:anjun [6p]) (rs:ankou [9p])))
        (list (rs:minjun [6p] [6p] player)
              (list (rs:antoi [1p]) (rs:anjun [1p])
                    (rs:anjun [4p]) (rs:ankou [9p]))))
      (do-test [7p]
        (list (rs:minjun [5p] [7p] player)
              (list (rs:ankou [1p]) (rs:anjun [2p])
                    (rs:anjun [7p]) (rs:antoi [9p])))
        (list (rs:minjun [7p] [7p] player)
              (list (rs:ankou [1p]) (rs:anjun [2p])
                    (rs:anjun [5p]) (rs:antoi [9p]))))
      (do-test [8p]
        (list (rs:mintoi [8p] player)
              (list (rs:ankou [1p]) (rs:anjun [2p])
                    (rs:anjun [5p]) (rs:ankou [9p]))))
      (do-test [9p]
        (list (rs:minjun [7p] [9p] player)
              (list (rs:antoi [1p]) (rs:anjun [1p])
                    (rs:anjun [4p]) (rs:ankou [9p])))
        (list (rs:minkou [9p] player)
              (list (rs:antoi [1p]) (rs:anjun [1p])
                    (rs:anjun [4p]) (rs:anjun [7p]))))))
  )

;;; Ordering tests - daisharin

(defparameter *daisharin-tiles*
  (rt:read-tile-list-from-string "22334455667788p"))

(define-test find-orderings-daisharin-tsumo
  (flet ((do-test (tile &rest orderings)
           (let* ((new-tiles (remove tile *daisharin-tiles*
                                     :test #'rt:tile= :count 1)))
             (apply #'test-orderings
                    (make-test-hand :winning-tile tile :free-tiles new-tiles)
                    orderings))))
    (do-test [2p]
      (list (rs:antoi [2p])
            (list (rs:antoi [3p]) (rs:antoi [4p]) (rs:antoi [5p])
                  (rs:antoi [6p]) (rs:antoi [7p]) (rs:antoi [8p])))
      (list (rs:antoi [2p])
            (list (rs:anjun [3p]) (rs:anjun [3p])
                  (rs:anjun [6p]) (rs:anjun [6p])))
      (list (rs:anjun [2p])
            (list (rs:antoi [5p]) (rs:anjun [2p])
                  (rs:anjun [6p]) (rs:anjun [6p])))
      (list (rs:anjun [2p])
            (list (rs:antoi [8p]) (rs:anjun [2p])
                  (rs:anjun [5p]) (rs:anjun [5p]))))
    (do-test [3p]
      (list (rs:antoi [3p])
            (list (rs:antoi [2p]) (rs:antoi [4p]) (rs:antoi [5p])
                  (rs:antoi [6p]) (rs:antoi [7p]) (rs:antoi [8p])))
      (list (rs:anjun [3p])
            (list (rs:antoi [2p]) (rs:anjun [3p])
                  (rs:anjun [6p]) (rs:anjun [6p])))
      (list (rs:anjun [2p])
            (list (rs:antoi [5p]) (rs:anjun [2p])
                  (rs:anjun [6p]) (rs:anjun [6p])))
      (list (rs:anjun [2p])
            (list (rs:antoi [8p]) (rs:anjun [2p])
                  (rs:anjun [5p]) (rs:anjun [5p]))))
    (do-test [4p]
      (list (rs:antoi [4p])
            (list (rs:antoi [2p]) (rs:antoi [3p]) (rs:antoi [5p])
                  (rs:antoi [6p]) (rs:antoi [7p]) (rs:antoi [8p])))
      (list (rs:anjun [3p])
            (list (rs:antoi [2p]) (rs:anjun [3p])
                  (rs:anjun [6p]) (rs:anjun [6p])))
      (list (rs:anjun [2p])
            (list (rs:antoi [5p]) (rs:anjun [2p])
                  (rs:anjun [6p]) (rs:anjun [6p])))
      (list (rs:anjun [2p])
            (list (rs:antoi [8p]) (rs:anjun [2p])
                  (rs:anjun [5p]) (rs:anjun [5p]))))
    (do-test [5p]
      (list (rs:antoi [5p])
            (list (rs:antoi [2p]) (rs:antoi [3p]) (rs:antoi [4p])
                  (rs:antoi [6p]) (rs:antoi [7p]) (rs:antoi [8p])))
      (list (rs:antoi [5p])
            (list (rs:anjun [2p]) (rs:anjun [2p])
                  (rs:anjun [6p]) (rs:anjun [6p])))
      (list (rs:anjun [3p])
            (list (rs:antoi [2p]) (rs:anjun [3p])
                  (rs:anjun [6p]) (rs:anjun [6p])))
      (list (rs:anjun [5p])
            (list (rs:antoi [8p]) (rs:anjun [2p])
                  (rs:anjun [2p]) (rs:anjun [5p]))))
    (do-test [6p]
      (list (rs:antoi [6p])
            (list (rs:antoi [2p]) (rs:antoi [3p]) (rs:antoi [4p])
                  (rs:antoi [5p]) (rs:antoi [7p]) (rs:antoi [8p])))
      (list (rs:anjun [6p])
            (list (rs:antoi [2p]) (rs:anjun [3p])
                  (rs:anjun [3p]) (rs:anjun [6p])))
      (list (rs:anjun [6p])
            (list (rs:antoi [5p]) (rs:anjun [2p])
                  (rs:anjun [2p]) (rs:anjun [6p])))
      (list (rs:anjun [5p])
            (list (rs:antoi [8p]) (rs:anjun [2p])
                  (rs:anjun [2p]) (rs:anjun [5p]))))
    (do-test [7p]
      (list (rs:antoi [7p])
            (list (rs:antoi [2p]) (rs:antoi [3p]) (rs:antoi [4p])
                  (rs:antoi [5p]) (rs:antoi [6p]) (rs:antoi [8p])))
      (list (rs:anjun [6p])
            (list (rs:antoi [2p]) (rs:anjun [3p])
                  (rs:anjun [3p]) (rs:anjun [6p])))
      (list (rs:anjun [6p])
            (list (rs:antoi [5p]) (rs:anjun [2p])
                  (rs:anjun [2p]) (rs:anjun [6p])))
      (list (rs:anjun [5p])
            (list (rs:antoi [8p]) (rs:anjun [2p])
                  (rs:anjun [2p]) (rs:anjun [5p]))))
    (do-test [8p]
      (list (rs:antoi [8p])
            (list (rs:antoi [2p]) (rs:antoi [3p]) (rs:antoi [4p])
                  (rs:antoi [5p]) (rs:antoi [6p]) (rs:antoi [7p])))
      (list (rs:antoi [8p])
            (list (rs:anjun [2p]) (rs:anjun [2p])
                  (rs:anjun [5p]) (rs:anjun [5p])))
      (list (rs:anjun [6p])
            (list (rs:antoi [2p]) (rs:anjun [3p])
                  (rs:anjun [3p]) (rs:anjun [6p])))
      (list (rs:anjun [6p])
            (list (rs:antoi [5p]) (rs:anjun [2p])
                  (rs:anjun [2p]) (rs:anjun [6p]))))))

(define-test find-orderings-daisharin-ron
  (do-all-other-players (player)
    (flet ((do-test (tile &rest orderings)
             (let* ((new-tiles (remove tile *daisharin-tiles*
                                       :test #'rt:tile= :count 1)))
               (apply #'test-orderings
                      (make-test-hand :class 'rh:closed-ron-hand
                                      :winning-tile tile :free-tiles new-tiles
                                      :taken-from player)
                      orderings))))
      (do-test [2p]
        (list (rs:mintoi [2p] player)
              (list (rs:antoi [3p]) (rs:antoi [4p]) (rs:antoi [5p])
                    (rs:antoi [6p]) (rs:antoi [7p]) (rs:antoi [8p])))
        (list (rs:mintoi [2p] player)
              (list (rs:anjun [3p]) (rs:anjun [3p])
                    (rs:anjun [6p]) (rs:anjun [6p])))
        (list (rs:minjun [2p] [2p] player)
              (list (rs:antoi [5p]) (rs:anjun [2p])
                    (rs:anjun [6p]) (rs:anjun [6p])))
        (list (rs:minjun [2p] [2p] player)
              (list (rs:antoi [8p]) (rs:anjun [2p])
                    (rs:anjun [5p]) (rs:anjun [5p]))))
      (do-test [3p]
        (list (rs:mintoi [3p] player)
              (list (rs:antoi [2p]) (rs:antoi [4p]) (rs:antoi [5p])
                    (rs:antoi [6p]) (rs:antoi [7p]) (rs:antoi [8p])))
        (list (rs:minjun [3p] [3p] player)
              (list (rs:antoi [2p]) (rs:anjun [3p])
                    (rs:anjun [6p]) (rs:anjun [6p])))
        (list (rs:minjun [2p] [3p] player)
              (list (rs:antoi [5p]) (rs:anjun [2p])
                    (rs:anjun [6p]) (rs:anjun [6p])))
        (list (rs:minjun [2p] [3p] player)
              (list (rs:antoi [8p]) (rs:anjun [2p])
                    (rs:anjun [5p]) (rs:anjun [5p]))))
      (do-test [4p]
        (list (rs:mintoi [4p] player)
              (list (rs:antoi [2p]) (rs:antoi [3p]) (rs:antoi [5p])
                    (rs:antoi [6p]) (rs:antoi [7p]) (rs:antoi [8p])))
        (list (rs:minjun [3p] [4p] player)
              (list (rs:antoi [2p]) (rs:anjun [3p])
                    (rs:anjun [6p]) (rs:anjun [6p])))
        (list (rs:minjun [2p] [4p] player)
              (list (rs:antoi [5p]) (rs:anjun [2p])
                    (rs:anjun [6p]) (rs:anjun [6p])))
        (list (rs:minjun [2p] [4p] player)
              (list (rs:antoi [8p]) (rs:anjun [2p])
                    (rs:anjun [5p]) (rs:anjun [5p]))))
      (do-test [5p]
        (list (rs:mintoi [5p] player)
              (list (rs:antoi [2p]) (rs:antoi [3p]) (rs:antoi [4p])
                    (rs:antoi [6p]) (rs:antoi [7p]) (rs:antoi [8p])))
        (list (rs:mintoi [5p] player)
              (list (rs:anjun [2p]) (rs:anjun [2p])
                    (rs:anjun [6p]) (rs:anjun [6p])))
        (list (rs:minjun [3p] [5p] player)
              (list (rs:antoi [2p]) (rs:anjun [3p])
                    (rs:anjun [6p]) (rs:anjun [6p])))
        (list (rs:minjun [5p] [5p] player)
              (list (rs:antoi [8p]) (rs:anjun [2p])
                    (rs:anjun [2p]) (rs:anjun [5p]))))
      (do-test [6p]
        (list (rs:mintoi [6p] player)
              (list (rs:antoi [2p]) (rs:antoi [3p]) (rs:antoi [4p])
                    (rs:antoi [5p]) (rs:antoi [7p]) (rs:antoi [8p])))
        (list (rs:minjun [6p] [6p] player)
              (list (rs:antoi [2p]) (rs:anjun [3p])
                    (rs:anjun [3p]) (rs:anjun [6p])))
        (list (rs:minjun [6p] [6p] player)
              (list (rs:antoi [5p]) (rs:anjun [2p])
                    (rs:anjun [2p]) (rs:anjun [6p])))
        (list (rs:minjun [5p] [6p] player)
              (list (rs:antoi [8p]) (rs:anjun [2p])
                    (rs:anjun [2p]) (rs:anjun [5p]))))
      (do-test [7p]
        (list (rs:mintoi [7p] player)
              (list (rs:antoi [2p]) (rs:antoi [3p]) (rs:antoi [4p])
                    (rs:antoi [5p]) (rs:antoi [6p]) (rs:antoi [8p])))
        (list (rs:minjun [6p] [7p] player)
              (list (rs:antoi [2p]) (rs:anjun [3p])
                    (rs:anjun [3p]) (rs:anjun [6p])))
        (list (rs:minjun [6p] [7p] player)
              (list (rs:antoi [5p]) (rs:anjun [2p])
                    (rs:anjun [2p]) (rs:anjun [6p])))
        (list (rs:minjun [5p] [7p] player)
              (list (rs:antoi [8p]) (rs:anjun [2p])
                    (rs:anjun [2p]) (rs:anjun [5p]))))
      (do-test [8p]
        (list (rs:mintoi [8p] player)
              (list (rs:antoi [2p]) (rs:antoi [3p]) (rs:antoi [4p])
                    (rs:antoi [5p]) (rs:antoi [6p]) (rs:antoi [7p])))
        (list (rs:mintoi [8p] player)
              (list (rs:anjun [2p]) (rs:anjun [2p])
                    (rs:anjun [5p]) (rs:anjun [5p])))
        (list (rs:minjun [6p] [8p] player)
              (list (rs:antoi [2p]) (rs:anjun [3p])
                    (rs:anjun [3p]) (rs:anjun [6p])))
        (list (rs:minjun [6p] [8p] player)
              (list (rs:antoi [5p]) (rs:anjun [2p])
                    (rs:anjun [2p]) (rs:anjun [6p])))))))

;;; Ordering tests - kokushi musou and puutaa

(define-test find-orderings-closed-kokushi-musou-shiisan-puutaa
  (dolist (tile rs:*kokushi-musou-tiles*)
    (test-orderings
     (make-test-hand :winning-tile tile
                     :free-tiles rs:*kokushi-musou-tiles*)
     (list (rs:closed-kokushi-musou tile))
     (list (rs:shiisan-puutaa tile (remove tile rs:*kokushi-musou-tiles*
                                           :test #'rt:tile=))))))

(define-test find-orderings-open-kokushi-musou
  (do-all-other-players (player)
    (dolist (pair-tile rs:*kokushi-musou-tiles*)
      (dolist (open-tile rs:*kokushi-musou-tiles*)
        (let ((free-tiles (cons pair-tile
                                (remove open-tile rs:*kokushi-musou-tiles*
                                        :count 1 :test #'rt:tile=))))
          (test-orderings
           (make-test-hand :winning-tile open-tile
                           :class 'rh:closed-ron-hand
                           :free-tiles free-tiles
                           :taken-from player)
           (list (rs:open-kokushi-musou pair-tile open-tile player))))))))

(define-test find-orderings-shiisuu-puutaa
  (test-orderings
   (make-test-hand :winning-tile [5m]
                   :free-tiles rs:*kokushi-musou-tiles*)
   (list (rs:shiisuu-puutaa (cons [5m] rs:*kokushi-musou-tiles*)))))

;;; TODO tests with locked sets in the hand
