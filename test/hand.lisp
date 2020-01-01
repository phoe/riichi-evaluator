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
                       &key class prevailing-wind seat-wind winning-tile
                         locked-sets free-tiles dora-list situations
                       &allow-other-keys)
  (let* ((known-keys '(:class :prevailing-wind :seat-wind :winning-tile
                       :locked-sets :free-tiles :dora-list :situations))
         (other-keys (apply #'a:remove-from-plist args known-keys)))
    (apply #'make-instance (or class 'rh:closed-tsumo-hand)
           :prevailing-wind (or prevailing-wind :east)
           :seat-wind (or seat-wind :east)
           :winning-tile (or winning-tile [1p])
           :locked-sets locked-sets
           :free-tiles (or free-tiles
                           (rt:read-tile-list-from-string "1112345678999p"))
           :dora-list (or dora-list '([3p]))
           :situations situations
           other-keys)))

(define-test hand-negative
  (fail (make-test-hand :prevailing-wind :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :seat-wind :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :winning-tile :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :locked-sets :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :locked-sets '(:keyword)) 'rh:invalid-hand-element)
  (fail (make-test-hand :free-tiles :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :free-tiles '(:keyword)) 'rh:invalid-hand-element)
  (fail (make-test-hand :dora-list :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :dora-list '(:keyword)) 'rh:invalid-hand-element)
  (fail (make-test-hand :situations :keyword) 'rh:invalid-hand-element)
  (fail (make-test-hand :situations '(42)) 'rh:invalid-hand-element)
  (fail (make-test-hand :situations '((42))) 'rh:invalid-hand-element)
  (fail (make-test-hand :class 'rh:closed-ron-hand
                        :losing-player :keyword)
      'rh:invalid-hand-element)
  (fail (make-test-hand :class 'rh:closed-ron-hand
                        :losing-player :toimen
                        :ura-dora-list :keyword)
      'rh:invalid-hand-element)
  (fail (make-test-hand :class 'rh:closed-ron-hand
                        :losing-player :toimen
                        :ura-dora-list '(:keyword))
      'rh:invalid-hand-element)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "11112345678999p"))
      'rh:invalid-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "1112345678999p")
         :locked-sets (list (rs:read-set-from-string "456p")))
      'rh:invalid-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "112345678999p"))
      'rh:invalid-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "1112345699999p"))
      'rh:invalid-same-tile-count)
  (fail (make-test-hand
         :free-tiles (rt:read-tile-list-from-string "1112378999p")
         :locked-sets (list (rs:read-set-from-string "456*p")))
      'rh:minjun-invalid-meld))
