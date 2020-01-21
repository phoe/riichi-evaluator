;;;; test/yaku.lisp
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

;;; Fu counting



;;; Yaku counting

(define-test riichi-ura-dora-length
  ;; TODO: we allow the ura dora list to be empty since at this point
  ;; we do not know if riichi was declared.
  ;; Introduce the typechecks for that in situations.lisp.
  ;; (fail (make-test-hand :ura-dora-list '()) 'rh:invalid-dora-list-length)
  ;; (fail (make-test-hand :dora-list '([4p]) :ura-dora-list '([5p] [6p]))
  ;;     'rh:invalid-dora-list-lengths)
  ;; (fail (make-test-hand :dora-list '([4p] [5p]) :ura-dora-list '([6p]))
  ;;     'rh:invalid-dora-list-lengths)
  )
