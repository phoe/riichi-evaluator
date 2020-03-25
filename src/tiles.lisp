;;;; src/tiles.lisp
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

(defpackage #:riichi-evaluator.tiles
  (:use #:cl
        #:riichi-evaluator.constants)
  (:local-nicknames
   (#:a #:alexandria)
   (#:nr #:named-readtables)
   (#:p #:protest/base))
  (:export
   ;; Conditions
   #:invalid-tile-datum
   ;; Tile protocol
   #:tile #:tile-p #:suited-p #:simple-p #:terminal-p #:honor-p #:wind-p
   #:dragon-p #:of-suit #:of-rank #:of-wind #:of-dragon #:tile= #:tile-consec-p
   #:tile< #:suit #:rank #:kind
   ;; Concrete classes
   #:suited-tile #:honor-tile
   ;; Tile list reader and printer
   #:print-tile-list #:read-tile-list #:read-tile-list-from-string
   ;; Convenience
   #:make-tile #:tile-list=
   ))

(in-package #:riichi-evaluator.tiles)

;;; Conditions

(define-condition invalid-tile-datum (type-error riichi-evaluator-error) ())

;;; Tile protocol

(p:define-protocol-class tile () ())

(defmethod make-load-form ((tile tile) &optional env)
  (make-load-form-saving-slots tile :environment env))

(defgeneric tile-p (tile)
  (:method (tile) nil)
  (:method ((tile tile)) t))

(defgeneric suited-p (tile)
  (:method ((tile tile)) nil))

(defgeneric simple-p (tile)
  (:method ((tile tile)) nil))

(defgeneric terminal-p (tile)
  (:method ((tile tile)) nil))

(defgeneric honor-p (tile)
  (:method ((tile tile)) nil))

(defgeneric wind-p (tile)
  (:method ((tile tile)) nil))

(defgeneric dragon-p (tile)
  (:method ((tile tile)) nil))

(defgeneric of-suit (suit tile)
  (:method (suit (tile tile)) nil))

(defgeneric of-rank (rank tile)
  (:method (rank (tile tile)) nil))

(defgeneric of-wind (wind tile)
  (:method (wind (tile tile)) nil))

(defgeneric of-dragon (dragon tile)
  (:method (dragon (tile tile)) nil))

(defgeneric tile= (tile1 tile2)
  (:method ((tile1 tile) (tile2 tile)) nil))

(defgeneric tile-consec-p (tile1 tile2 &key wrap-around)
  (:method ((tile1 tile) (tile2 tile) &key wrap-around)
    (declare (ignore wrap-around))
    nil))

(defgeneric tile< (tile1 tile2))

;;; Suited tile

(defclass suited-tile (tile)
  ((%suit :initarg :suit
          :reader suit)
   (%rank :initarg :rank
          :reader rank)))

(defmethod initialize-instance :after ((tile suited-tile) &key suit rank)
  (unless (member suit *suits*)
    (error 'invalid-tile-datum
           :datum suit
           :expected-type '#.`(member ,@*suits*)))
  (unless (and (numberp rank)
               (<= 1 rank 9))
    (error 'invalid-tile-datum
           :datum rank :expected-type '(integer 1 9))))

(defmethod print-object ((tile suited-tile) stream)
  (handler-case
      (format stream "[~D~A]" (rank tile)
              (a:assoc-value *lisp-print-table* (suit tile)))
    (error () (format stream "[##]"))))

(defmethod suited-p ((tile suited-tile)) t)

(defmethod terminal-p ((tile suited-tile))
  (member (rank tile) '(1 9) :test #'=))

(defmethod simple-p ((tile suited-tile))
  (not (terminal-p tile)))

(defmethod of-suit (suit (tile suited-tile))
  (equal suit (suit tile)))

(defmethod of-rank (rank (tile suited-tile))
  (equal rank (rank tile)))

(defmethod tile= ((a suited-tile) (b suited-tile))
  (and (of-rank (rank a) b)
       (of-suit (suit a) b)))

(defmethod tile< ((a suited-tile) (b suited-tile))
  (let ((pos-a (position (suit a) *suits*))
        (pos-b (position (suit b) *suits*)))
    (or (< pos-a pos-b)
        (and (= pos-a pos-b) (< (rank a) (rank b))))))

(defmethod tile-consec-p ((t1 suited-tile) (t2 suited-tile) &key wrap-around)
  (and (of-suit (suit t1) t2)
       (let* ((rank (rank t1))
              (next-rank (if (and wrap-around (= 9 rank)) 1 (1+ rank))))
         (of-rank next-rank t2))))

;;; Honor tile

(defclass honor-tile (tile)
  ((%kind :initarg :kind
          :reader kind)))

(defmethod initialize-instance :before ((tile honor-tile) &key kind)
  (unless (member kind *honors*)
    (error 'invalid-tile-datum :datum kind
                               :expected-type '#.`(member ,@*honors*))))

(defmethod rank ((tile honor-tile)) (1+ (position (kind tile) *honors*)))

;; NOTE: Convenience method. Note that honor tiles are nonetheless not SUITED-P.
(defmethod suit ((tile honor-tile)) :honor)

(defmethod honor-p ((tile honor-tile)) t)

(defmethod wind-p ((tile honor-tile)) (position (kind tile) *winds*))

(defmethod dragon-p ((tile honor-tile)) (position (kind tile) *dragons*))

(defmethod print-object ((tile honor-tile) stream)
  (handler-case
      (format stream "[~A]"
              (a:assoc-value *lisp-print-table* (kind tile)))
    (error () (format stream "[##]"))))

(defmethod of-wind (wind (tile honor-tile))
  (equal wind (kind tile)))

(defmethod of-dragon (dragon (tile honor-tile))
  (equal dragon (kind tile)))

(defmethod tile= ((a honor-tile) (b honor-tile))
  (equal (kind a) (kind b)))

(defmethod tile< ((a honor-tile) (b honor-tile))
  (let ((pos-a (position (kind a) *honors*))
        (pos-b (position (kind b) *honors*)))
    (< pos-a pos-b)))

(defmethod tile-consec-p ((t1 honor-tile) (t2 honor-tile) &key wrap-around)
  (flet ((next-with-rollover (item table)
           (a:if-let ((pos (position item table)))
             (elt table (mod (1+ pos) (length table)))
             (return-from tile-consec-p nil))))
    (when wrap-around
      (let* ((table (cond ((wind-p t1) *winds*)
                          ((dragon-p t1) *dragons*)))
             (succ (next-with-rollover (kind t1) table)))
        (equal succ (kind t2))))))

;;; Sorting order for suited and honor tiles

(defmethod tile< ((a suited-tile) (b honor-tile)) t)

(defmethod tile< ((a honor-tile) (b suited-tile)) nil)

;;; Tile reader

(defun tile-reader (stream char)
  (declare (ignore char))
  (labels
      ((reader-error (control &rest args)
         (error 'a:simple-reader-error
                :stream stream
                :format-control control :format-arguments args)))
    (let* ((char-1 (read-char stream t nil t))
           (char-2 (read-char stream t nil t))
           (final-char (read-char stream t nil t)))
      (unless (find char-1 "123456789ESWNWGR" :test #'char-equal)
        (reader-error "Invalid first tile character ~@C." char-1))
      (unless (find char-2 "MPSWDZ" :test #'char-equal)
        (reader-error "Invalid second tile character ~@C." char-2))
      (unless (eql #\] final-char)
        (reader-error "Expected a #\] but got ~@C instead." final-char))
      (cond
        ;; Suited tile.
        ((and (digit-char-p char-1) (<= 1 (digit-char-p char-1) 9)
              (member char-2 *suit-read-table* :key #'cdr))
         (let* ((suit (a:rassoc-value *suit-read-table* char-2
                                      :test #'char-equal)))
           (make-instance 'suited-tile :suit suit
                                       :rank (digit-char-p char-1))))
        ;; Wind tile.
        ((and (eql #\W char-2) (member char-1 *wind-read-table* :key #'cdr))
         (let ((wind (a:rassoc-value *wind-read-table* char-1
                                     :test #'char-equal)))
           (make-instance 'honor-tile :kind wind)))
        ;; Dragon tile.
        ((and (eql #\D char-2) (member char-1 *dragon-read-table* :key #'cdr))
         (let ((wind (a:rassoc-value *dragon-read-table* char-1
                                     :test #'char-equal)))
           (make-instance 'honor-tile :kind wind)))
        ;; Numeric honor tile.
        ((and (digit-char-p char-1) (<= 1 (digit-char-p char-1) 7)
              (char-equal #\z char-2))
         (let ((kind (nth (1- (digit-char-p char-1)) *honors*)))
           (make-instance 'honor-tile :kind kind)))
        ;; Invalid tile.
        (t (reader-error "Attempted to read an unknown tile [~C~C]."
                         char-1 char-2))))))

(nr:defreadtable :riichi-evaluator
  (:merge :standard)
  (:macro-char #\[ 'tile-reader)
  (:macro-char #\] (get-macro-character #\))))

;;; Tile list reader and printer

(defun read-tile-list-from-string (string)
  (with-input-from-string (stream string)
    (read-tile-list stream)))

(defun read-tile-list (stream)
  (loop for char = (peek-char nil stream nil nil t)
        with ranks = '()
        while (and char (alphanumericp char))
        do (setf char (read-char stream t nil t))
        if (digit-char-p char)
          do (push (digit-char-p char) ranks)
        else
          nconc
          (loop with suit = (a:rassoc-value *print-table* char)
                for rank in (nreverse ranks)
                collect (if (eq suit :honor)
                            (let ((kind (nth (1- rank) *honors*)))
                              (make-instance 'honor-tile :kind kind))
                            (make-instance 'suited-tile :rank rank :suit suit))
                finally (setf ranks '()))
          into result
        finally (return (sort result #'tile<))))

(defun print-tile-list (tiles &optional (stream t)
                              ;; NOTE: the following optional arguments are for
                              ;; convenience of printing tile lists from inside
                              ;; set.lisp. If you use them, the list will
                              ;; no longer be readable using READ-TILE-LIST.
                                (flipped-positions '())
                                (shouminkan-positions '()))
  (labels
      ((thunk (stream)
         (loop with last-suit = (suit (first tiles))
               for tile in tiles
               for i from 0
               for suit = (if (suited-p tile) (suit tile) :honor)
               unless (eq suit last-suit)
                 do (princ (a:assoc-value *print-table* last-suit) stream)
                    (setf last-suit suit)
               do (princ (rank tile) stream)
               when (member i flipped-positions)
                 do (princ "*" stream)
               when (member i shouminkan-positions)
                 do (princ "**" stream)
               finally (princ (a:assoc-value *print-table* suit) stream))))
    (case stream
      ((t) (thunk *standard-output*))
      ((nil) (with-output-to-string (stream) (thunk stream)))
      (t (thunk stream)))))

;;; Convenience functions

(defun make-tile (string)
  (first (read-tile-list-from-string string)))

(defun tile-list= (list-1 list-2)
  (and (= (length list-1) (length list-2))
       (loop for tile-1 in (sort (copy-list list-1) #'tile<)
             for tile-2 in (sort (copy-list list-2) #'tile<)
             always (tile= tile-1 tile-2))))
