;;;; src/payment-trainer.lisp
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

;;; Simple payment trainer

(defparameter *ten-scoring-values*
  (nconc (mapcar (a:curry #'list 1) (a:iota 9 :start 30 :step 10))
         (mapcar (a:curry #'list 2) (a:iota 10 :start 20 :step 10))
         (mapcar (a:curry #'list 3) (a:iota 5 :start 20 :step 10))
         (mapcar (a:curry #'list 4) (a:iota 2 :start 20 :step 10))))

(defun ten (han fu &key oya-p ron-p)
  (let* ((base-ten (* fu (expt 2 (+ 2 han))))
         (ten (cond ((and (not ron-p) (not oya-p)) (* 1 base-ten))
                    ((and (not ron-p) oya-p)       (* 2 base-ten))
                    ((and ron-p       (not oya-p)) (* 4 base-ten))
                    ((and ron-p       oya-p)       (* 6 base-ten)))))
    (* 100 (ceiling ten 100))))

(defun compute-ten-list (han fu)
  (list (ten han fu)
        (ten han fu :oya-p t)
        (ten han fu :ron-p t)
        (ten han fu :oya-p t :ron-p t)))

(defun test-check-answer (actual expected)
  (cond ((equal actual expected)
         (format *query-io* (a:whichever "Good!" "Nice!" "Awesome!"))
         t)
        (t
         (format *query-io* "Wrong! ~{~D~^ ~}" expected))))

(defun test-han-fu->ten ()
  (destructuring-bind (han fu) (a:random-elt *ten-scoring-values*)
    (format *query-io* "~&~D han ~D fu is: " han fu)
    (let ((actual (loop repeat 4 collect (read *query-io*)))
          (expected (compute-ten-list han fu)))
      (test-check-answer actual expected))))

(defun test-ten->han-fu ()
  (let* ((datum (a:random-elt *ten-scoring-values*))
         (expected (apply #'compute-ten-list datum)))
    (format *query-io* "~&~{~D~^ ~} ten is: " expected)
    (let ((actual (apply #'compute-ten-list
                         (loop repeat 2 collect (read *query-io*)))))
      (test-check-answer actual expected))))

(defun train-ten (&optional (max 10))
  (loop with correct = 0
        for all from 0 below max
        when (test-han-fu->ten)
          ;; (a:whichever (test-han-fu->ten) (test-ten->han-fu))
          do (incf correct)
        finally (format *query-io* "~&Correct answers: ~D out of ~D."
                        correct all)))