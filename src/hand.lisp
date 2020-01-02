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
                    (#:p #:protest/base))
  (:export
   ;; Conditions
   #:invalid-hand #:invalid-hand-element
   #:invalid-dora-list-length #:invalid-situation
   #:invalid-tile-count #:invalid-same-tile-count #:minjun-invalid-meld
   #:invalid-dora-list-lengths
   ;; Protocol
   #:hand #:prevailing-wind #:seat-wind #:winning-tile #:locked-sets
   #:free-tiles #:dora-list #:situations #:hand-total-visible-tiles
   #:validate-situation
   #:tsumo-hand #:ron-hand #:open-hand #:closed-hand
   #:ura-dora-list #:losing-player
   ;; Concrete classes
   #:open-tsumo-hand #:open-ron-hand #:closed-tsumo-hand #:closed-ron-hand
   ))

(in-package #:riichi-evaluator.hand)

;;; Conditions

(define-condition invalid-hand (riichi-evaluator-error)
  ((%hand :reader invalid-hand-hand :initarg :hand))
  (:default-initargs
   :hand (a:required-argument :hand)))

(define-condition invalid-hand-element (invalid-hand type-error) ())

(define-condition invalid-dora-list-length (invalid-hand)
  ((%dora-list :reader invalid-dora-list :initarg :dora-list)
   (%ura-dora-p :reader invalid-dora-list-ura-dora-p :initarg :ura-dora-p))
  (:default-initargs
   :dora-list (a:required-argument :dora-list)
   :ura-dora-p nil)
  (:report
   (lambda (condition stream)
     (format stream "The ~Adora list ~S is not between 1 and 5 elements long."
             (if (invalid-dora-list-ura-dora-p condition) "ura-" "")
             (invalid-dora-list condition)))))

(defun invalid-dora-list-length (hand dora-list &optional ura-dora-p)
  (error 'invalid-dora-list-length :hand hand :dora-list dora-list
                                   :ura-dora-p ura-dora-p))

(define-condition invalid-situation (invalid-hand simple-condition)
  ((%situation :reader invalid-situation-situation :initarg :situation))
  (:default-initargs
   :situation (a:required-argument :situation)
   :format-control "No reason given.")
  (:report
   (lambda (condition stream)
     (let ((situation (invalid-situation-situation condition)))
       (format stream "Invalid situation ~S for hand ~S:~%~A"
               (if (and (consp situation) (null (cdr situation)))
                   (car situation)
                   situation)
               (invalid-situation-hand condition)
               (apply #'format nil
                      (simple-condition-format-control condition)
                      (simple-condition-format-arguments condition)))))))

(defun invalid-situation (hand situation args format-control &rest format-args)
  (error 'invalid-situation :hand hand :situation (cons situation args)
                            :format-control format-control
                            :format-args format-args))

(define-condition invalid-tile-count (invalid-hand)
  ((%expected :reader invalid-tile-count-expected :initarg :expected)
   (%actual :reader invalid-tile-count-actual :initarg :actual))
  (:default-initargs
   :expected (a:required-argument :expected)
   :actual (a:required-argument :actual))
  (:report (lambda (condition stream)
             (format stream "The hand has ~D tiles, but ~D were expected."
                     (invalid-tile-count-actual condition)
                     (invalid-tile-count-expected condition)))))

(define-condition invalid-same-tile-count (invalid-hand)
  ((%tile :reader invalid-same-tile-count-tile :initarg :tile)
   (%count :reader invalid-same-tile-count-count :initarg :count))
  (:default-initargs
   :tile (a:required-argument :tile)
   :count (a:required-argument :count))
  (:report (lambda (condition stream)
             (format stream
                     "There are ~D ~A tiles visible, but at most 4 are allowed."
                     (invalid-same-tile-count-count condition)
                     (invalid-same-tile-count-tile condition)))))

(define-condition minjun-invalid-meld (invalid-hand)
  ((%taken-from :reader minjun-invalid-meld-taken-from :initarg :taken-from)
   (%set :reader minjun-invalid-meld-set :initarg :set))
  (:default-initargs
   :tiles (a:required-argument :tiles)
   :set (a:required-argument :set))
  (:report
   (lambda (condition stream)
     (format stream "The hand ~A contains a non-winning minjun ~A with a tile ~
                     taken from ~(~A~) instead of kamicha."
             (invalid-hand-hand condition)
             (minjun-invalid-meld-set condition)
             (minjun-invalid-meld-taken-from condition)))))

(define-condition invalid-dora-list-lengths (invalid-hand) ()
  (:report
   (lambda (condition stream)
     (let ((hand (invalid-hand-hand condition)))
       (format stream "The dora list ~S and ura dora list ~S for hand ~S ~
                       are not of the same length."
               (dora-list hand) (ura-dora-list hand) hand)))))

;;; Hand

(p:define-protocol-class hand ()
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

(defgeneric hand-total-visible-tiles (hand)
  (:method-combination append))

(defmethod hand-total-visible-tiles append ((hand hand))
  (append (list (winning-tile hand))
          (a:mappend #'tiles (locked-sets hand))
          (free-tiles hand)
          (dora-list hand)))

(defgeneric validate-situation (hand situation &rest args)
  (:method-combination progn))

(defun check-hand-elt-type (hand datum expected-type)
  (unless (typep datum expected-type)
    (error 'invalid-hand-element :hand hand :datum datum
                                 :expected-type expected-type)))

(defun check-hand-elt-type-list (hand list expected-type
                                 &optional min-length max-length ura-dora-p)
  (check-hand-elt-type hand list 'list)
  (when (or min-length max-length)
    (let ((length (length list)))
      (when (or (and min-length (< length min-length))
                (and max-length (< max-length length)))
        (invalid-dora-list-length hand list ura-dora-p))))
  (dolist (elt list)
    (check-hand-elt-type hand elt expected-type)))

(defun check-hand-situations (hand)
  (dolist (situation (situations hand))
    (if (listp situation)
        (apply #'validate-situation hand situation)
        (validate-situation hand situation))))

(defun check-tile-count (hand)
  (loop for set in (locked-sets hand)
        for (set-count set-extra) = (multiple-value-list (set-tile-count set))
        sum set-count into set-sum
        sum set-extra into set-extra-sum
        finally
           (let ((expected-tile-count (+ 14 set-extra-sum))
                 (actual-tile-count (+ 1 (length (free-tiles hand)) set-sum)))
             (unless (= expected-tile-count actual-tile-count)
               (error 'invalid-tile-count :hand hand
                                          :expected expected-tile-count
                                          :actual actual-tile-count)))))

(defun check-at-most-four-tiles-of-a-kind (hand)
  (let ((tiles (hand-total-visible-tiles hand)))
    (loop for tile in (remove-duplicates tiles :test #'tile=)
          for count = (count-if (a:curry #'tile= tile) tiles)
          unless (<= count 4)
            do (error 'invalid-same-tile-count
                      :hand hand :tile tile :count count))))

(defun check-locked-minjuns-taken-from-kamicha (hand)
  (dolist (set (locked-sets hand))
    (when (typep set 'minjun)
      (let ((taken-from (taken-from set)))
        (unless (eq :kamicha taken-from)
          (error 'minjun-invalid-meld :hand hand :set set
                                      :taken-from taken-from))))))

(defmethod initialize-instance :after ((hand hand) &key)
  (check-hand-elt-type hand (prevailing-wind hand) '#.`(member ,@*winds*))
  (check-hand-elt-type hand (seat-wind hand) '#.`(member ,@*winds*))
  (check-hand-elt-type hand (winning-tile hand) 'tile)
  (check-hand-elt-type-list hand (locked-sets hand) 'set)
  (check-hand-elt-type-list hand (free-tiles hand) 'tile)
  (check-hand-elt-type hand (dora-list hand) 'list)
  (check-hand-elt-type-list hand (dora-list hand) 'tile 1 5)
  (check-hand-elt-type-list hand (situations hand)
                            `(or keyword (cons keyword))))

(defmethod initialize-instance :around ((hand hand) &key)
  (prog1 (call-next-method)
    (check-hand-situations hand)
    (check-tile-count hand)
    (check-at-most-four-tiles-of-a-kind hand)
    (check-locked-minjuns-taken-from-kamicha hand)))

(p:define-protocol-class tsumo-hand (hand) ())
(p:define-protocol-class ron-hand (hand)
  ((%losing-player :accessor losing-player :initarg :losing-player))
  (:default-initargs
   :losing-player (a:required-argument :losing-player)))

(defmethod initialize-instance :after ((hand ron-hand) &key)
  (check-hand-elt-type hand (losing-player hand)
                       '#.`(member ,@*other-players*)))

(p:define-protocol-class open-hand (hand) ())
(p:define-protocol-class closed-hand (hand)
  ((%ura-dora-list :accessor ura-dora-list :initarg :ura-dora-list))
  (:default-initargs
   :ura-dora-list '()))

(defun check-dora-ura-dora-list-length (hand)
  (let ((dora-list-length (length (dora-list hand)))
        (ura-dora-list-length (length (ura-dora-list hand))))
    (unless (= dora-list-length ura-dora-list-length)
      (error 'invalid-dora-list-lengths :hand hand))))

(defmethod initialize-instance :after ((hand closed-hand) &key)
  (check-hand-elt-type hand (ura-dora-list hand) 'list)
  (check-hand-elt-type-list hand (ura-dora-list hand) 'tile 1 5 t)
  (check-dora-ura-dora-list-length hand))

(defmethod hand-total-visible-tiles append ((hand closed-hand))
  (ura-dora-list hand))

;;; Concrete classes

(defclass open-tsumo-hand (tsumo-hand open-hand) ())
(defclass open-ron-hand (ron-hand open-hand) ())
(defclass closed-tsumo-hand (tsumo-hand closed-hand) ())
(defclass closed-ron-hand (ron-hand closed-hand) ())
