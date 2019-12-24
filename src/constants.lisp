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
  (:export
   ;; Variables
   #:*suit-table* #:*wind-table* #:*dragon-table* #:*dragon-print-table*
   #:*honor-table* #:*other-players* #:*tile-list-map*
   ;; Conditions
   #:riichi-evaluator-error))

(in-package #:riichi-evaluator.constants)

(defparameter *suit-table*
  '((#\M . :number)
    (#\P . :circle)
    (#\S . :bamboo)))

(defparameter *wind-table*
  '((#\E . :east)
    (#\S . :south)
    (#\W . :west)
    (#\N . :north)))

(defparameter *dragon-table*
  '((#\B . :haku)
    (#\F . :hatsu)
    (#\C . :chun)))

;; TODO move this into methods instead
(defparameter *dragon-print-table*
  '(("Hk" . :haku)
    ("Ht" . :hatsu)
    ("Ch" . :chun)))

(defparameter *honor-table*
  (append *wind-table* *dragon-table*))

(defparameter *other-players*
  '(:shimocha :toimen :kamicha))

(defparameter *tile-list-map*
  '((:number . #\m)
    (:circle . #\p)
    (:bamboo . #\s)
    (:honor . #\z)))

(define-condition riichi-evaluator-error (error) ())
