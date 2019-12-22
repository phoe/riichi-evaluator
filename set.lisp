;;;; set.lisp
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

(defpackage #:riichi-evaluator.set
  (:use #:cl
        #:riichi-evaluator.tiles)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base))
  (:shadow #:set))

(in-package #:riichi-evaluator.set)

;;; Data

(defparameter *other-players*
  '(:shimocha :toimen :kamicha))

;;; Conditions

(define-condition invalid-set-element (type-error) ())

(define-condition invalid-tile-taken-from (type-error) ())

(define-condition open-tile-not-in-set ()
  ((%open-tile :reader open-tile :initarg :open-tile)
   (%tiles :reader tiles :initarg :tiles))
  (:default-initargs
   :open-tile (a:required-argument :open-tile)
   :tiles (a:required-argument :tiles))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to make a set whose open tile ~A is not in ~A."
             (open-tile condition) (tiles condition)))))

(define-condition invalid-shuntsu ()
  ((%tiles :reader tiles :initarg :tiles))
  (:default-initargs
   :tiles (a:required-argument :tiles))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to make a shuntsu with non-consecutive tiles ~A."
             (tiles condition)))))

;;; Protocol

(p:define-protocol-class set ()
  ((%count :reader set-count :initarg :count))
  (:default-initargs :count (a:required-argument :count)))

(p:define-protocol-class same-tile-set (set)
  ((%tile :reader same-tile-set-tile :initarg :tile))
  (:default-initargs
   :tile (a:required-argument :same-tile-set-tile)))

(defmethod initialize-instance :after ((set same-tile-set) &key)
  (let ((tile (same-tile-set-tile set)))
    (unless (tile-p tile)
      (error 'invalid-set-element :datum tile :expected-type 'tile))))

(defgeneric tiles (set)
  (:method ((set same-tile-set))
    (make-list (set-count set) :initial-element (same-tile-set-tile set))))

(p:define-protocol-class closed-set-mixin () ())
(p:define-protocol-class open-set-mixin ()
  ((%taken-from :reader open-set-taken-from :initarg :taken-from))
  (:default-initargs
   :taken-from (a:required-argument :taken-from)))

(defmethod initialize-instance :after ((set open-set-mixin) &key)
  (let ((taken-from (open-set-taken-from set)))
    (unless (member taken-from *other-players*)
      (error 'invalid-tile-taken-from
             :datum taken-from
             :expected-type `(member ,*other-players*)))))

(p:define-protocol-class shuntsu ()
  ((%tiles :reader set-tiles :initarg :tiles))
  (:default-initargs
   :count 3
   :tiles (a:required-argument :tiles)))

(defmethod initialize-instance :after ((set shuntsu) &key)
  (let ((tiles (tiles set)))
    (dolist (tile tiles)
      (unless (tile-p tile)
        (error 'invalid-set-element :datum tile :expected-type 'tile)))
    (setf (slot-value set '%tiles) (sort tiles #'tile<))
    (destructuring-bind (tile-1 tile-2 tile-3) tiles
      (unless (and (tile-consec-p tile-1 tile-2) (tile-consec-p tile-2 tile-3))
        (error 'invalid-shuntsu :tiles tiles)))))

(p:define-protocol-class toitsu (same-tile-set) ()
  (:default-initargs :count 2))
(p:define-protocol-class koutsu (same-tile-set) ()
  (:default-initargs :count 3))
(p:define-protocol-class kantsu (same-tile-set) ()
  (:default-initargs :count 4))

;;; Concrete classes

(defclass antoi (toitsu closed-set-mixin) ())
(defclass mintoi (toitsu open-set-mixin) ())

(defclass anjun (shuntsu closed-set-mixin) ())
(defclass minjun (shuntsu open-set-mixin)
  ((%open-tile :reader open-set-tile :initarg :open-tile))
  (:default-initargs
   :open-tile (a:required-argument :open-tile)))

(defmethod initialize-instance :after ((set minjun) &key)
  (let ((tile (open-tile set))
        (tiles (tiles set)))
    (unless (member tile tiles)
      (error 'open-tile-not-in-set :open-tile tile :tiles tiles))))

(defclass ankou (koutsu closed-set-mixin) ())
(defclass minkou (koutsu open-set-mixin) ())

(defclass ankan (kantsu closed-set-mixin) ())
(defclass daiminkan (kantsu open-set-mixin) ())
(defclass shouminkan (kantsu open-set-mixin) ())
