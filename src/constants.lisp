;;;; src/constants.lisp
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

(defpackage #:riichi-evaluator.constants
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   ;; Variables
   #:*suits* #:*winds* #:*dragons* #:*honors* #:*other-players*
   #:*suit-read-table* #:*wind-read-table* #:*dragon-read-table*
   #:*print-table* #:*lisp-print-table*
   ;; Conditions
   #:riichi-evaluator-error
   ;; Utils
   #:multiple-value-or #:bag-difference
   ;; Method combinations
   #:chained-or #:chained-append))

(in-package #:riichi-evaluator.constants)

(defparameter *suits* '(:number :circle :bamboo))
(defparameter *winds* '(:east :south :west :north))
(defparameter *dragons* '(:haku :hatsu :chun))
(defparameter *honors* (append *winds* *dragons*))
(defparameter *other-players* '(:shimocha :toimen :kamicha))

(defparameter *suit-read-table*
  '((:number . #\m)
    (:circle . #\p)
    (:bamboo . #\s)))

(defparameter *wind-read-table*
  '((:east . #\E)
    (:south . #\S)
    (:west . #\W)
    (:north . #\N)))

(defparameter *dragon-read-table*
  '((:haku . #\W)
    (:hatsu . #\G)
    (:chun . #\R)))

(defparameter *print-table*
  '((:number . #\m)
    (:circle . #\p)
    (:bamboo . #\s)
    (:honor . #\z)))

(defparameter *lisp-print-table*
  '((:number . #\m) (:circle . #\p) (:bamboo . #\s)
    (:east . "EW")  (:south . "SW") (:west . "WW") (:north . "NW")
    (:haku . "WD")  (:hatsu . "GD") (:chun . "RD")))

(define-condition riichi-evaluator-error (error) ())

(defun bag-difference (bag-1 bag-2 &key (test 'eql))
  (loop with result = (copy-list bag-1)
        for element in bag-2
        if (member element result :test test)
          do (setf result (delete element result
                                  :test test :count 1))
        finally (return result)))

(defmacro multiple-value-or (&body forms)
  (when forms
    (destructuring-bind (first . rest) forms
      (a:with-gensyms (values primary-value)
        `(let* ((,values (multiple-value-list ,first))
                (,primary-value (first ,values)))
           (declare (ignorable ,primary-value))
           ,(if rest
                `(if ,primary-value
                     (values-list ,values)
                     (multiple-value-or ,@rest))
                `(values-list ,values)))))))

(define-method-combination chained-or ()
  ((methods *))
  (let ((calls (mapcar #'(lambda (x) `(call-method ,x)) methods)))
    `(multiple-value-or ,@calls)))

(define-method-combination chained-append ()
  ((methods *))
  (let ((calls (mapcar #'(lambda (x) `(call-method ,x)) methods)))
    `(append ,@calls)))
