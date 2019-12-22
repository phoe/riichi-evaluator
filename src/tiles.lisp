;;;; tiles.lisp
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
   ;; Tile protocol
   #:tile #:tile-p #:suited-p #:simple-p #:terminal-p #:honor-p #:wind-p #:dragon-p
   #:of-suit #:of-rank #:of-wind #:tile= #:tile-consec-p #:tile< #:format-tile
   ;; Conditions
   #:invalid-tile-datum
   ;; Concrete classes
   #:suited-tile #:honor-tile))

(in-package #:riichi-evaluator.tiles)

;;; Tile protocol

(p:define-protocol-class tile () ())

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

(defgeneric tile= (tile1 tile2)
  (:method ((tile1 tile) (tile2 tile)) nil))

(defgeneric tile-consec-p (tile1 tile2 &key wrap-around)
  (:method ((tile1 tile) (tile2 tile) &key wrap-around)
    (declare (ignore wrap-around))
    nil))

(defgeneric tile< (tile1 tile2))

(defgeneric format-tile (tile))

;;; Conditions

(define-condition invalid-tile-datum (type-error) ())

;;; Suited tile

(defclass suited-tile (tile)
  ((%suit :initarg :suit
          :reader suit)
   (%rank :initarg :rank
          :reader rank)))

(defmethod initialize-instance :after ((tile suited-tile) &key)
  (unless (member (suit tile) *suit-table* :key #'cdr)
    (error 'invalid-tile-datum
           :datum (suit tile) :expected-type `(member ,@(mapcar #'cdr *suit-table*))))
  (unless (and (numberp (rank tile))
               (<= 1 (rank tile) 9))
    (error 'invalid-tile-datum
           :datum (rank tile) :expected-type '(integer 1 9))))

(defmethod print-object ((tile suited-tile) stream)
  (handler-case (format stream "[~D~C]"
                        (rank tile)
                        (char-downcase (a:rassoc-value *suit-table* (suit tile))))
    (error () (format stream "[##]"))))

(defmethod suited-p ((tile suited-tile))
  t)

(defmethod terminal-p ((tile suited-tile))
  (member (rank tile) '(1 9)))

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
  (let ((pos-a (position (suit a) *suit-table* :key #'cdr))
        (pos-b (position (suit b) *suit-table* :key #'cdr)))
    (or (< pos-a pos-b)
        (and (= pos-a pos-b) (< (rank a) (rank b))))))

(defmethod tile-consec-p ((t1 suited-tile) (t2 suited-tile) &key wrap-around)
  (and (of-suit (suit t1) t2)
       (let* ((rank (rank t1))
              (next-rank (if (and wrap-around (= 9 rank)) 1 (1+ rank))))
         (of-rank next-rank t2))))

(defmethod format-tile ((tile suited-tile))
  (format nil "~c~d" (a:rassoc-value *suit-table* (suit tile)) (rank tile)))

;;; Honor tile

(defclass honor-tile (tile)
  ((%kind :initarg :kind
          :reader kind)))

(defmethod initialize-instance :after ((tile honor-tile) &key)
  (unless (member (kind tile) *honor-table* :key #'cdr)
    (error 'invalid-tile-datum
           :datum (kind tile) :expected-type `(member ,@(mapcar #'cdr *honor-table*)))))

(defmethod honor-p ((tile honor-tile))
  t)

(defmethod wind-p ((tile honor-tile))
  (position (kind tile) *wind-table* :key #'cdr))

(defmethod dragon-p ((tile honor-tile))
  (position (kind tile) *dragon-table* :key #'cdr))

(defmethod print-object ((tile honor-tile) stream)
  (handler-case (let ((text (cond ((wind-p tile)
                                   (a:rassoc-value *honor-table* (kind tile)))
                                  ((dragon-p tile)
                                   (a:rassoc-value *dragon-print-table* (kind tile))))))
                  (format stream "[~A]" text))
    (error () (format stream "[##]"))))

(defmethod of-wind (wind (tile honor-tile))
  (equal wind (kind tile)))

(defmethod tile= ((a honor-tile) (b honor-tile))
  (equal (kind a) (kind b)))

(defmethod tile< ((a honor-tile) (b honor-tile))
  (let ((pos-a (position (kind a) *honor-table* :key #'cdr))
        (pos-b (position (kind b) *honor-table* :key #'cdr)))
    (< pos-a pos-b)))

(defmethod format-tile ((tile honor-tile))
  (a:rassoc-value *honor-table* (kind tile)))

(defmethod tile-consec-p ((t1 honor-tile) (t2 honor-tile) &key wrap-around)
  (flet ((next-with-rollover (item table)
           (a:if-let ((pos (position item table)))
             (elt table (mod (1+ pos) (length table)))
             (return-from tile-consec-p nil))))
    (when wrap-around
      (let* ((table (cond ((wind-p t1) (mapcar #'cdr *wind-table*))
                          ((dragon-p t1) (mapcar #'cdr *dragon-table*))))
             (succ (next-with-rollover (kind t1) table)))
        (equal succ (kind t2))))))

;;; Sorting order for suited and honor tiles.

(defmethod tile< ((a suited-tile) (b honor-tile)) t)

(defmethod tile< ((a honor-tile) (b suited-tile)) nil)

;;; Tile list printer.

;; (defun format-tile-list (tiles)
;;   (loop with sorted-tiles = (sort (copy-list tiles) #'tile<)
;;         for current-suit = 
;;         for tile in sorted-tiles))

;;; Lisp reader for tiles.

(nr:defreadtable :riichi-evaluator
  (:merge :standard))

(nr:in-readtable :riichi-evaluator)

(defun tile-reader (stream char)
  (declare (ignore char))
  (labels ((reader-error (control &rest args)
             (error 'a:simple-reader-error
                    :stream stream
                    :format-control control :format-arguments args))
           (check-final-char ()
             (let ((final-char (read-char stream t nil t)))
               (unless (eql #\] final-char)
                 (reader-error "Expected a #\] but got ~@C instead." final-char)))))
    (let* ((char (read-char stream t nil t))
           (digit (digit-char-p char))
           next-char
           result)
      (cond
        ((eql digit 0)
         ;; "0 of circles" is actually a meme in the Cracow Chombo Club.
         (reader-error "Attempted to read a 0-rank tile."))
        (digit
         ;; Suited tile.
         (let* ((suit-char (read-char stream t nil t))
                (suit (or (a:assoc-value *suit-table* suit-char :test #'char-equal)
                          (reader-error "Invalid suit character: ~@C" suit-char))))
           (setf result (make-instance 'suited-tile :suit suit :rank digit))))
        ((eql #\] (setf next-char (peek-char nil stream t nil t)))
         ;; Wind tile.
         (let ((wind (or (a:assoc-value *wind-table* char :test #'char-equal)
                         (reader-error "Invalid wind character: ~@C" next-char))))
           (setf result (make-instance 'honor-tile :kind wind))))
        (t
         ;; Dragon tile.
         (setf next-char (read-char stream t nil t))
         (let* ((string (coerce (list char next-char) 'string))
                (dragon (or (a:assoc-value *dragon-print-table* string :test #'string-equal)
                            (reader-error "Invalid dragon character: ~@C" next-char))))
           (setf result (make-instance 'honor-tile :kind dragon)))))
      (check-final-char)
      result)))

(set-macro-character #\[ 'tile-reader nil (nr:find-readtable :riichi-evaluator))

(set-macro-character #\] (get-macro-character #\)) nil (nr:find-readtable :riichi-evaluator))
