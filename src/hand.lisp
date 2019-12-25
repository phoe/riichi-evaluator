;;;; src/hand.lisp
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

(defpackage #:riichi-evaluator.hand
  (:use #:cl
        #:riichi-evaluator.constants
        #:riichi-evaluator.tiles
        #:riichi-evaluator.set)
  (:shadowing-import-from #:riichi-evaluator.set
                          #:set)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)))

(in-package #:riichi-evaluator.hand)

;;; Conditions

(define-condition invalid-hand (riichi-evaluator-error) ())

(define-condition invalid-hand-element (type-error riichi-evaluator-error) ())

(define-condition invalid-situation (invalid-hand)
  ((%hand :reader invalid-situation-hand :initarg :hand)
   (%situation :reader invalid-situation-situation :initarg :situation)
   (%args :reader invalid-args-args :initarg :args)
   (%reason :reader invalid-situation-reason :initarg :reason))
  (:default-initargs
   :hand (a:required-argument :hand)
   :situation (a:required-argument :situation)
   :args '()
   :reason "No reason given.")
  (:report (lambda (condition stream)
             (format stream "Invalid situation ~S for hand ~S: ~A"
                     (invalid-situation-hand condition)
                     (invalid-situation-situation condition)
                     (invalid-situation-reason condition)))))

;;; Hand

(defclass hand ()
  ((%prevailing-wind :accessor prevailing-wind :initarg :prevailing-wind)
   (%seat-wind :accessor seat-wind :initarg :seat-wind)
   (%winning-tile :accessor winning-tile :initarg :winning-tile)
   (%locked-sets :accessor locked-sets :initarg :locked-sets)
   (%free-tiles :accessor free-tiles :initarg :free-tiles)
   (%dora-list :accessor dora-list :initarg :dora-list)
   (%situations :accessor situations :initarg :situations))
  (:default-initargs
   :prevailing-wind :east :seat-wind :east
   :winning-tile (a:required-argument :winning-tile)
   :locked-sets (a:required-argument :locked-sets)
   :free-tiles (a:required-argument :free-tiles)
   :dora-list (a:required-argument :dora-list)
   :situations '()))

(defgeneric validate-situation (hand situation &rest args)
  (:method ((hand hand) situation &rest args)
    (error 'invalid-situation :situation situation :hand hand :args args
                              :reason "Unknown situation.")))

;; TODO validate that there are at most 4 tiles of any given kind in a hand.

(defun check-hand-element-type (datum expected-type)
  (unless (typep datum expected-type)
    (error 'invalid-hand-element :datum datum
                                 :expected-type expected-type)))

(defun check-hand-element-type-list (list expected-type)
  (check-hand-element-type list 'list)
  (dolist (elt list)
    (check-hand-element-type elt expected-type)))

(defmethod initialize-instance :after ((hand hand) &key)
  (check-hand-element-type (prevailing-wind hand) '#.`(member ,@*winds*))
  (check-hand-element-type (seat-wind hand) '#.`(member ,@*winds*))
  (check-hand-element-type (winning-tile hand) 'tile)
  (check-hand-element-type-list (locked-sets hand) 'set)
  (check-hand-element-type-list (free-tiles hand) 'tile)
  (check-hand-element-type-list (dora-list hand) 'tile)
  (check-hand-element-type-list (situations hand) '(or keyword (cons keyword)))
  (dolist (situation (situations hand))
    (if (listp situation)
        (apply #'validate-situation hand situation)
        (validate-situation hand situation))))

(p:define-protocol-class tsumo-hand () ())
(p:define-protocol-class ron-hand () ())
(p:define-protocol-class open-hand () ())
(p:define-protocol-class closed-hand ()
  ((ura-dora-list :accessor hand-ura-dora-list :initarg :ura-dora))
  (:default-initargs :ura-dora-list '()))

(defclass open-tsumo-hand (open-hand tsumo-hand) ())
(defclass open-ron-hand (open-hand ron-hand) ())
(defclass closed-tsumo-hand (closed-hand tsumo-hand) ())
(defclass closed-ron-hand (closed-hand ron-hand) ())
