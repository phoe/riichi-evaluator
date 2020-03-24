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
                        :locked-sets (rs:make-set "123p"))
      'rh:closed-locked-set)
  (fail (make-test-hand :free-tiles
                        (rt:read-tile-list-from-string "2345678999p")
                        :locked-sets (rs:make-set "111p"))
      'rh:closed-locked-set)
  (fail (make-test-hand :free-tiles
                        (rt:read-tile-list-from-string "11123456789p")
                        :locked-sets (rs:make-set "99p"))
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
         :locked-sets (rs:make-set "4*56p"))
      'rh:invalid-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "112345678999p"))
      'rh:invalid-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "1112345699999p"))
      'rh:invalid-same-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "1112378999p")
         :locked-sets (rs:make-set "456*p"))
      'rh:minjun-invalid-meld)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "1112378999p")
         :locked-sets (rs:make-set "456p"))
      'rh:closed-locked-set)
  (fail (make-test-hand
         :class 'rh:open-ron-hand
         :taken-from :kamicha
         :free-tiles (rt:read-tile-list-from-string "1112345678999p"))
      'rh:open-hand-no-open-sets))

(define-test hand-positive
  (dolist (args `((:class rh:closed-tsumo-hand)
                  (:class rh:closed-ron-hand
                   :taken-from :toimen)
                  (:class rh:open-tsumo-hand
                   :free-tiles ,(rt:read-tile-list-from-string "1145678999p")
                   :locked-sets ,(rs:make-set "2*13p"))
                  (:class rh:open-ron-hand
                   :taken-from :toimen
                   :free-tiles ,(rt:read-tile-list-from-string "1145678999p")
                   :locked-sets ,(rs:make-set "2*13p"))))
    (let ((expected-closed-hand
            '([1p] [1p] [1p] [2p] [3p] [4p] [5p]
              [6p] [7p] [8p] [9p] [9p] [9p]))
          (expected-open-hand
            '([1p] [1p] [4p] [5p]
              [6p] [7p] [8p] [9p] [9p] [9p]))
          (expected-visible-tiles
            '([1p] [1p] [1p] [1p] [2p] [3p] [3p] [4p] [5p]
              [6p] [7p] [8p] [9p] [9p] [9p]))
          (hand (apply #'make-test-hand args)))
      (is eq :east (rh:prevailing-wind hand))
      (is eq :east (rh:seat-wind hand))
      (is rt:tile= [1p] (rh:winning-tile hand))
      (is rt:tile-list= '([3p]) (rh:dora-list hand))
      (is eq '() (rh:situations hand))
      (is rt:tile-list= expected-visible-tiles
          (sort (copy-list (rh:hand-total-visible-tiles hand)) #'rt:tile<))
      (typecase hand
        (rh:closed-tsumo-hand
         (is rt:tile-list= expected-closed-hand (rh:free-tiles hand))
         (is eq '() (rh:locked-sets hand))
         (is eq '() (rh:ura-dora-list hand)))
        (rh:closed-ron-hand
         (is rt:tile-list= expected-closed-hand (rh:free-tiles hand))
         (is eq '() (rh:locked-sets hand))
         (is eq :toimen (rh:taken-from hand))
         (is eq '() (rh:ura-dora-list hand)))
        (rh:open-tsumo-hand
         (is rt:tile-list= expected-open-hand
             (sort (copy-list (rh:free-tiles hand)) #'rt:tile<))
         (is = 1 (length (rh:locked-sets hand)))
         (is rs:set= (rs:minjun [1p] [2p] :kamicha)
             (first (rh:locked-sets hand))))
        (rh:open-ron-hand
         (is rt:tile-list= expected-open-hand
             (sort (copy-list (rh:free-tiles hand)) #'rt:tile<))
         (is = 1 (length (rh:locked-sets hand)))
         (is rs:set= (rs:minjun [1p] [2p] :kamicha)
             (first (rh:locked-sets hand)))
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

;;; Ordering tests - tatsumaki with locked sets

(define-test find-orderings-tatsumaki-locked-sets-tsumo
  (let ((locked-sets (list (rs:minjun [3p] [3p] :kamicha)
                           (rs:daiminkan [NW] :toimen))))
    (flet ((do-test (tile &rest orderings)
             (apply #'test-orderings
                    (make-test-hand
                     :class 'rh:open-tsumo-hand
                     :free-tiles (rt:read-tile-list-from-string "4445666s")
                     :locked-sets locked-sets
                     :winning-tile tile)
                    orderings)))
      (do-test [3s]
        (list (rs:anjun [3s])
              (list* (rs:antoi [4s]) (rs:ankou [6s]) locked-sets)))
      (do-test [4s]
        (list (rs:ankou [4s])
              (list* (rs:anjun [4s]) (rs:antoi [6s]) locked-sets))
        (list (rs:anjun [4s])
              (list* (rs:ankou [4s]) (rs:antoi [6s]) locked-sets)))
      (do-test [5s]
        (list (rs:antoi [5s])
              (list* (rs:ankou [4s]) (rs:ankou [6s]) locked-sets)))
      (do-test [6s]
        (list (rs:anjun [4s])
              (list* (rs:antoi [4s]) (rs:ankou [6s]) locked-sets))
        (list (rs:ankou [6s])
              (list* (rs:antoi [4s]) (rs:anjun [4s]) locked-sets)))
      (do-test [7s]
        (list (rs:anjun [5s])
              (list* (rs:ankou [4s]) (rs:antoi [6s]) locked-sets))))))

(define-test find-orderings-tatsumaki-locked-sets-ron
  (let ((locked-sets (list (rs:minjun [3p] [3p] :kamicha)
                           (rs:daiminkan [NW] :toimen))))
    (do-all-other-players (player)
      (flet ((do-test (tile &rest orderings)
               (apply #'test-orderings
                      (make-test-hand
                       :class 'rh:open-ron-hand
                       :free-tiles (rt:read-tile-list-from-string "4445666s")
                       :locked-sets locked-sets
                       :winning-tile tile
                       :taken-from player)
                      orderings)))
        (do-test [3s]
          (list (rs:minjun [3s] [3s] player)
                (list* (rs:antoi [4s]) (rs:ankou [6s]) locked-sets)))
        (do-test [4s]
          (list (rs:minkou [4s] player)
                (list* (rs:anjun [4s]) (rs:antoi [6s]) locked-sets))
          (list (rs:minjun [4s] [4s] player)
                (list* (rs:ankou [4s]) (rs:antoi [6s]) locked-sets)))
        (do-test [5s]
          (list (rs:mintoi [5s] player)
                (list* (rs:ankou [4s]) (rs:ankou [6s]) locked-sets)))
        (do-test [6s]
          (list (rs:minjun [4s] [6s] player)
                (list* (rs:antoi [4s]) (rs:ankou [6s]) locked-sets))
          (list (rs:minkou [6s] player)
                (list* (rs:antoi [4s]) (rs:anjun [4s]) locked-sets)))
        (do-test [7s]
          (list (rs:minjun [5s] [7s] player)
                (list* (rs:ankou [4s]) (rs:antoi [6s]) locked-sets)))))))

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

;;; Ordering tests - all machi from Arcturus

(defun test-machi (hand-tiles winning-tiles)
  (do-all-tiles (tile)
    (a:when-let
        ((hand (handler-case
                   (make-test-hand
                    :class 'rh:closed-tsumo-hand
                    :free-tiles (rt:read-tile-list-from-string hand-tiles)
                    :winning-tile tile
                    :dora-list '([NW]))
                 (rh:invalid-same-tile-count (e)
                   (true (rt:tile= tile (rh:invalid-same-tile-count-tile e)))
                   (true (= 5 (rh:invalid-same-tile-count-count e)))
                   nil))))
      (if (member tile winning-tiles :test #'rt:tile=)
          (true (rh:find-orderings hand))
          (false (rh:find-orderings hand))))))

;; TODO tile reader macro should accept [1z] syntax.

(define-test find-orderings-machi-ryanmen
  (test-machi "123456789m11z45s" '([3s] [6s])))

(define-test find-orderings-machi-penchan
  (test-machi "123456789m11z12s" '([3s])))

(define-test find-orderings-machi-shanpon
  (test-machi "123456789m11z22z" '([EW] [SW])))

(define-test find-orderings-machi-kanchan
  (test-machi "123456789m11z46s" '([5s])))

(define-test find-orderings-machi-tanki
  (test-machi "123456789m111z4s" '([4s])))

(define-test find-orderings-machi-nobetan
  (test-machi "123456789m3456s" '([3s] [6s])))

(define-test find-orderings-machi-nobetan
  (test-machi "123456m11z34567s" '([2s] [5s] [8s])))

(define-test find-orderings-machi-sanmentan
  (test-machi "123456m2345678s" '([2s] [5s] [8s])))

(define-test find-orderings-machi-entotsu
  (test-machi "123456m45666s11z" '([3s] [6s] [EW])))

(define-test find-orderings-machi-aryanmen
  (test-machi "123456789m4566s" '([3s] [6s])))

(define-test find-orderings-machi-ryantan
  (test-machi "123456789m5666s" '([4s] [5s] [7s])))

(define-test find-orderings-machi-pentan
  (test-machi "123456789m1222s" '([1s] [3s])))

(define-test find-orderings-machi-kantan
  (test-machi "123456789m1333s" '([1s] [2s])))

(define-test find-orderings-machi-kantankan
  (test-machi "123456m1113555s" '([2s] [3s] [4s])))

(define-test find-orderings-machi-tatsumaki
  (test-machi "123456m3334555s" '([2s] [3s] [4s] [5s] [6s])))

(define-test find-orderings-machi-complex-2-tile
  (test-machi "123456m3567888s" '([3s] [4s])))

(define-test find-orderings-machi-complex-3-tile
  (test-machi "123456m1113456s"  '([2s] [3s] [6s]))
  (test-machi "123456m4455566s"  '([4s] [5s] [6s]))
  (test-machi "123m22334455s11z" '([2s] [5s] [EW]))
  (test-machi "123456m1122223s"  '([1s] [3s] [4s]))
  (test-machi "123456m5567777s"  '([5s] [6s] [8s]))
  (test-machi "123456m3455777s"  '([2s] [5s] [6s]))
  (test-machi "123456m4556777s"  '([3s] [5s] [6s])))

(define-test find-orderings-machi-complex-4-tile
  (test-machi "123m2233445566s" '([2s] [3s] [5s] [6s]))
  (test-machi "123m34555s67888p" '([2s] [5s] [5p] [8p]))
  (test-machi "123m2223456677s" '([5s] [6s] [7s] [8s]))
  (test-machi "34555m22334455s" '([2m] [5m] [2s] [5s]))
  (test-machi "123456m4445566s" '([4s] [5s] [6s] [7s]))
  (test-machi "123456m2223334s" '([2s] [3s] [4s] [5s]))
  (test-machi "123m33345678s11z" '([3s] [6s] [9s] [EW]))
  (test-machi "123456m4555678s" '([3s] [4s] [6s] [9s]))
  (test-machi "123456m2233334s" '([1s] [2s] [4s] [5s]))
  (test-machi "123456m4566777s" '([3s] [5s] [6s] [8s])))

(define-test find-orderings-machi-complex-5-tile
  (test-machi "123m2345556677s" '([2s] [5s] [6s] [7s] [8s]))
  (test-machi "123456m3334567s" '([2s] [4s] [5s] [7s] [8s]))
  (test-machi "123m4445667888s" '([4s] [5s] [6s] [7s] [8s])))

(define-test find-orderings-machi-complex-6-tile
  (test-machi "123m1112345678s" '([2s] [3s] [5s] [6s] [8s] [9s]))
  (test-machi "2233444555666s"  '([1s] [2s] [3s] [4s] [5s] [6s]))
  (test-machi "123m2344567888s" '([1s] [3s] [4s] [6s] [7s] [9s]))
  (test-machi "123m2344445678s" '([2s] [3s] [5s] [6s] [8s] [9s]))
  (test-machi "2233445556667s"  '([2s] [3s] [5s] [6s] [7s] [8s]))
  (test-machi "123m4445666678s" '([3s] [4s] [5s] [7s] [8s] [9s])))

(define-test find-orderings-machi-complex-7-tile
  (test-machi "2333345677778s"  '([1s] [2s] [4s] [5s] [6s] [8s] [9s]))
  (test-machi "2344445666678s"  '([1s] [2s] [3s] [5s] [7s] [8s] [9s]))
  (test-machi "2233445566777s"  '([1s] [2s] [3s] [4s] [5s] [6s] [7s]))
  (test-machi "2344455566678s"  '([1s] [2s] [4s] [5s] [6s] [8s] [9s])))

(define-test find-orderings-machi-complex-8-tile
  (test-machi "2333344567888s"  '([1s] [2s] [4s] [5s] [6s] [7s] [8s] [9s]))
  (test-machi "2344445678999s"  '([1s] [2s] [3s] [5s] [6s] [7s] [8s] [9s]))
  (test-machi "123m2223456777s" '([1s] [2s] [3s] [4s] [5s] [6s] [7s] [8s])))
