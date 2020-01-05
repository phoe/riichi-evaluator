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
  (fail (make-test-hand :class 'rh:closed-ron-hand :losing-player :keyword)
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
                   :losing-player :toimen)
                  (:class rh:open-tsumo-hand)
                  (:class rh:open-ron-hand
                   :losing-player :toimen)))
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
         (is eq :toimen (rh:losing-player hand))
         (is eq '() (rh:ura-dora-list hand)))
        (rh:open-tsumo-hand)
        (rh:open-ron-hand
         (is eq :toimen (rh:losing-player hand)))))))

;;; Ordering finder

(defun ordering= (ordering-1 ordering-2)
  (destructuring-bind (winning-set-1 &optional other-sets-1) ordering-1
    (destructuring-bind (winning-set-2 &optional other-sets-2) ordering-2
      (and (rs:set= winning-set-1 winning-set-2)
           (= (length other-sets-1) (length other-sets-2))
           (loop with result = (copy-list other-sets-2)
                 for set in other-sets-1
                 do (a:deletef result set :test #'rs:set=)
                 finally (return (null result)))))))

(defun test-orderings (hand &rest expected-orderings)
  (let ((actual-orderings (rh:find-orderings hand)))
    (dolist (ordering actual-orderings)
      (true (find ordering expected-orderings :test #'ordering=)))
    (dolist (ordering expected-orderings)
      (true (find ordering actual-orderings :test #'ordering=)))))

(define-test find-orderings-nine-gates
  (flet ((do-test (tile &rest orderings)
           (apply #'test-find-orderings
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

(define-test find-orderings-daisharin
  (test-orderings
   (make-test-hand
    :winning-tile [2p]
    :free-tiles (rt:read-tile-list-from-string "2334455667788p"))
   (list (rs:antoi [2p])
         (list (rs:antoi [8p]) (rs:antoi [7p]) (rs:antoi [6p])
               (rs:antoi [5p]) (rs:antoi [4p]) (rs:antoi [3p])))
   (list (rs:antoi [2p])
         (list (rs:anjun [6p]) (rs:anjun [6p])
               (rs:anjun [3p]) (rs:anjun [3p])))
   (list (rs:anjun [2p])
         (list (rs:antoi [5p]) (rs:anjun [6p])
               (rs:anjun [6p]) (rs:anjun [2p])))
   (list (rs:anjun [2p])
         (list (rs:antoi [8p]) (rs:anjun [5p])
               (rs:anjun [5p]) (rs:anjun [2p])))))

(define-test find-orderings-kokushi-musou
  (dolist (tile rs:*kokushi-musou-tiles*)
    (test-orderings
     (make-test-hand :winning-tile tile
                     :free-tiles rs:*kokushi-musou-tiles*)
     (list (rs:closed-kokushi-musou tile))
     (list (rs:shiisan-puutaa tile (remove tile rs:*kokushi-musou-tiles*
                                           :test #'rt:tile=))))))

(define-test find-orderings-shiisuu-puutaa
  (test-orderings
   (make-test-hand :winning-tile [5m]
                   :free-tiles rs:*kokushi-musou-tiles*)
   (list (rs:shiisuu-puutaa (cons [5m] rs:*kokushi-musou-tiles*)))))
