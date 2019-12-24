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
        #:riichi-evaluator.tiles
        #:riichi-evaluator.set)
  (:shadowing-import-from #:riichi-evaluator.set
                          #:set)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)))

(in-package #:riichi-evaluator.hand)

;;; Hand

(defclass hand ()
  ((prevailing-wind :accessor hand-prevailing-wind :initarg :prevailing-wind)
   (seat-wind :accessor hand-seat-wind :initarg :seat-wind)
   (winning-tile :accessor hand-winning-tile :initarg :winning-tile)
   (locked-sets :accessor hand-locked-sets :initarg :locked-sets)
   (free-tiles :accessor hand-free-tiles :initarg :free-tiles)
   (dora-list :accessor hand-dora-list :initarg :dora-list))
  (:default-initargs
   :prevailing-wind :east :seat-wind :east
   :winning-tile (a:required-argument :winning-tile)
   :locked-sets (a:required-argument :locked-sets)
   :free-tiles (a:required-argument :free-tiles)
   :dora-list (a:required-argument :dora-list)))

(p:define-protocol-class tsumo-hand () ())
(p:define-protocol-class ron-hand () ())
(p:define-protocol-class open-hand () ())
(p:define-protocol-class closed-hand ()
  ((ura-dora-list :accessor hand-ura-dora-list :initarg :ura-dora)
   ;; TODO: rework riichi-p into a list of hand states, including rinshan,
   ;; daburi, chankan, etc..
   (riichi-p :accessor hand-riichi-p :initarg :riichi-p))
  (:default-initargs :riichi-p nil :ura-dora-list '()))

(defclass open-tsumo-hand (open-hand tsumo-hand) ())
(defclass open-ron-hand (open-hand ron-hand) ())
(defclass closed-tsumo-hand (closed-hand tsumo-hand) ())
(defclass closed-ron-hand (closed-hand ron-hand) ())
