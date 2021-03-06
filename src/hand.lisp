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
   #:invalid-hand #:invalid-hand-hand #:invalid-hand-element
   #:invalid-dora-list-length #:invalid-situation
   #:invalid-tile-count #:invalid-same-tile-count #:invalid-same-tile-count-tile
   #:invalid-same-tile-count-count #:minjun-invalid-meld
   #:closed-locked-set #:open-hand-no-open-sets
   ;; Protocol
   #:hand #:prevailing-wind #:seat-wind #:winning-tile #:locked-sets
   #:free-tiles #:dora-list #:situations #:hand-total-visible-tiles
   #:validate-situation
   #:tsumo-hand #:ron-hand #:open-hand #:closed-hand
   #:ura-dora-list #:taken-from
   ;; Concrete classes
   #:open-tsumo-hand #:open-ron-hand #:closed-tsumo-hand #:closed-ron-hand
   ;; Ordering finder
   #:ordering=
   #:find-winning-combinations #:find-winning-combinations-in-hand
   #:find-sets-in-free-tiles #:find-orderings))

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

(define-condition invalid-tile-count (invalid-hand)
  ((%expected :reader invalid-tile-count-expected :initarg :expected)
   (%actual :reader invalid-tile-count-actual :initarg :actual))
  (:default-initargs
   :expected (a:required-argument :expected)
   :actual (a:required-argument :actual))
  (:report
   (lambda (condition stream)
     (format stream "The hand has ~D tiles, but ~D were expected."
             (invalid-tile-count-actual condition)
             (invalid-tile-count-expected condition)))))

(define-condition invalid-same-tile-count (invalid-hand)
  ((%tile :reader invalid-same-tile-count-tile :initarg :tile)
   (%count :reader invalid-same-tile-count-count :initarg :count))
  (:default-initargs
   :tile (a:required-argument :tile)
   :count (a:required-argument :count))
  (:report
   (lambda (condition stream)
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

(define-condition closed-locked-set (invalid-hand)
  ((%locked-sets :reader locked-sets :initarg :locked-sets))
  (:default-initargs
   :locked-sets (a:required-argument :locked-sets))
  (:report
   (lambda (condition stream)
     (format stream "The hand ~S contains closed locked non-kan sets in its ~
                     list of locked sets ~S."
             (invalid-hand-hand condition)
             (locked-sets condition)))))

(define-condition open-hand-no-open-sets (invalid-hand)
  ((%locked-sets :reader locked-sets :initarg :locked-sets))
  (:default-initargs
   :locked-sets (a:required-argument :locked-sets))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to create an open hand ~S with no open sets in ~
                     the locked sets list ~S."
             (invalid-hand-hand condition)
             (locked-sets condition)))))

;;; Hand

;;; TODO: implement the count of riichi sticks and honba.
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
   :locked-sets '()
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

(defun check-no-closed-locked-sets (hand)
  (let ((locked-sets (locked-sets hand)))
    (dolist (set locked-sets)
      (when (typep set 'closed-set)
        (unless (typep set 'ankan)
          (error 'closed-locked-set :hand hand :locked-sets locked-sets))))))

(defmethod initialize-instance :before
    ((hand hand)
     &key prevailing-wind seat-wind winning-tile locked-sets
       free-tiles dora-list situations)
  (check-hand-elt-type hand prevailing-wind '#.`(member ,@*winds*))
  (check-hand-elt-type hand seat-wind '#.`(member ,@*winds*))
  (check-hand-elt-type hand winning-tile 'tile)
  (check-hand-elt-type-list hand locked-sets 'set)
  (check-hand-elt-type-list hand free-tiles 'tile)
  (check-hand-elt-type hand dora-list 'list)
  (check-hand-elt-type-list hand dora-list 'tile 1 5)
  (check-hand-elt-type-list hand situations `(or keyword (cons keyword))))

(defmethod initialize-instance :after ((hand hand) &key)
  (check-hand-situations hand)
  (check-tile-count hand)
  (check-at-most-four-tiles-of-a-kind hand)
  (check-locked-minjuns-taken-from-kamicha hand)
  (check-no-closed-locked-sets hand))

(p:define-protocol-class tsumo-hand (hand) ())
(p:define-protocol-class ron-hand (hand)
  ((%taken-from :accessor taken-from :initarg :taken-from))
  (:default-initargs
   :taken-from (a:required-argument :taken-from)))

(defmethod initialize-instance :before ((hand ron-hand) &key taken-from)
  (check-hand-elt-type hand taken-from
                       '#.`(member ,@*other-players*)))

(p:define-protocol-class open-hand (hand) ())

(defmethod initialize-instance :after ((hand open-hand) &key)
  (let ((sets (locked-sets hand)))
    (unless (find-if (a:rcurry #'typep 'open-set) sets)
      (error 'open-hand-no-open-sets :hand hand :locked-sets sets))))

(p:define-protocol-class closed-hand (hand)
  ((%ura-dora-list :accessor ura-dora-list :initarg :ura-dora-list))
  (:default-initargs
   :ura-dora-list '()))

(defmethod initialize-instance :before ((hand closed-hand) &key ura-dora-list)
  (check-hand-elt-type hand ura-dora-list 'list)
  ;; NOTE: we allow the ura dora list to be empty since at this point
  ;; we do not know if riichi was declared. This check needs to occur when the
  ;; riichi situation is validated.
  (check-hand-elt-type-list hand ura-dora-list 'tile 0 5 t))

(defmethod hand-total-visible-tiles append ((hand closed-hand))
  (ura-dora-list hand))

;;; Concrete classes

(defclass open-tsumo-hand (tsumo-hand open-hand) ())
(defclass open-ron-hand (ron-hand open-hand) ())
(defclass closed-tsumo-hand (tsumo-hand closed-hand) ())
(defclass closed-ron-hand (ron-hand closed-hand) ())

;;; Mahjong hand detection

(defun is-number-sets-of-type-p (count type sets)
  (= count (count-if (a:rcurry #'typep type) sets)))

(defgeneric mahjong-hand-p (sets)
  (:method-combination chained-or)
  (:method (sets) nil))

(defmethod mahjong-hand-p :standard (sets)
  (and (is-number-sets-of-type-p 4 'mentsu sets)
       (is-number-sets-of-type-p 1 'toitsu sets)
       (is-number-sets-of-type-p 0 '(not (or mentsu toitsu)) sets)))

(defmethod mahjong-hand-p :chiitoi (sets)
  (and (is-number-sets-of-type-p 7 'toitsu sets)
       (is-number-sets-of-type-p 0 '(not toitsu) sets)))

(defmethod mahjong-hand-p :full-hand-set (sets)
  (and (is-number-sets-of-type-p 1 'full-hand-set sets)
       (is-number-sets-of-type-p 0 '(not full-hand-set) sets)))

;;; Ordering finder

(defun ordering= (ordering-1 ordering-2)
  (destructuring-bind (winning-set-1 &optional other-sets-1) ordering-1
    (destructuring-bind (winning-set-2 &optional other-sets-2) ordering-2
      (and (set= winning-set-1 winning-set-2)
           (= (length other-sets-1) (length other-sets-2))
           (loop with result = (copy-list other-sets-2)
                 for set in other-sets-1
                 do (a:deletef result set :test #'set=)
                 finally (return (null result)))))))

(defun find-winning-combinations (free-tiles winning-tile win-from)
  (loop with forbidden-sets = '()
        for (set remaining-tiles)
          = (multiple-value-list
             (try-make-winning-set-from-tiles
              free-tiles winning-tile win-from forbidden-sets))
        while set
        collect (list set remaining-tiles)
        do (push set forbidden-sets)))

(defun find-winning-combinations-in-hand (hand)
  (find-winning-combinations (free-tiles hand) (winning-tile hand)
                             (etypecase hand
                               (tsumo-hand :tsumo)
                               (ron-hand (taken-from hand)))))

(defun find-sets-in-free-tiles
    (tiles &optional (forbidden-sets '()) (other-sets '()))
  (if (null tiles)
      (list other-sets)
      (multiple-value-bind (new-set new-tiles)
          (try-make-nonwinning-set-from-tiles tiles forbidden-sets)
        (when new-set
          (let ((orderings-with-new-set
                  (find-sets-in-free-tiles
                   new-tiles forbidden-sets (cons new-set other-sets)))
                (orderings-without-new-set
                  (find-sets-in-free-tiles
                   tiles (cons new-set forbidden-sets) other-sets)))
            (nconc orderings-with-new-set
                   orderings-without-new-set))))))

(defun find-orderings (hand &optional include-non-mahjong-orderings-p)
  (let ((winning-combinations (find-winning-combinations-in-hand hand))
        (possible-orderings '()))
    (dolist (combination winning-combinations)
      (destructuring-bind (winning-set remaining-tiles) combination
        (let ((set-combinations (find-sets-in-free-tiles remaining-tiles)))
          (dolist (free-sets set-combinations)
            (push (list winning-set (append free-sets (locked-sets hand)))
                  possible-orderings)))))
    (if include-non-mahjong-orderings-p
        possible-orderings
        (remove-if-not #'mahjong-hand-p possible-orderings
                       :key (lambda (x) (cons (first x)
                                              (second x)))))))
