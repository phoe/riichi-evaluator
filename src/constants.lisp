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
   #:*suits* #:*winds* #:*dragons* #:*honors* #:*other-players*
   #:*suit-read-table* #:*wind-read-table* #:*dragon-read-table*
   #:*print-table* #:*lisp-print-table*
   ;; Conditions
   #:riichi-evaluator-error))

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
