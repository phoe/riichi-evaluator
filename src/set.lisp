;;;; src/set.lisp
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
        #:riichi-evaluator.constants
        #:riichi-evaluator.tiles)
  (:local-nicknames (#:a #:alexandria)
                    (#:nr #:named-readtables)
                    (#:p #:protest/base)
                    (#:m #:closer-mop))
  (:shadow #:set)
  (:export
   ;; Conditions
   #:invalid-set-element #:invalid-tile-taken-from #:open-tile-not-in-set
   #:invalid-shuntsu #:invalid-kokushi-musou
   #:set-reader-error #:offending-string
   ;; Condition accessors
   #:open-tile #:tiles #:taken-from
   ;; Protocol
   #:set #:tiles #:set= #:set-tile-count
   #:same-tile-set #:closed-set #:open-set #:taken-from #:standard-set
   #:shuntsu #:toitsu #:koutsu #:kantsu
   #:full-hand-set #:twelve-singles-and-pair-set #:pair-tile
   #:kokushi-musou
   ;; Concrete classes
   #:antoi #:mintoi #:anjun #:minjun #:ankou #:minkou
   #:ankan #:daiminkan #:shouminkan
   ;; Set reader and printer
   #:print-set #:read-set #:read-set-from-string
   ;; Tile-set matcher
   #:try-make-set-from-tiles
   ))

(in-package #:riichi-evaluator.set)

(nr:in-readtable :riichi-evaluator)

;;; Conditions

(define-condition invalid-set-element (type-error riichi-evaluator-error) ())

(define-condition invalid-tile-taken-from (type-error riichi-evaluator-error)
  ())

(define-condition open-tile-not-in-set (riichi-evaluator-error)
  ((%open-tile :reader open-tile :initarg :open-tile)
   (%tiles :reader tiles :initarg :tiles))
  (:default-initargs
   :open-tile (a:required-argument :open-tile)
   :tiles (a:required-argument :tiles))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to make a set whose open tile ~A is not in ~A."
             (open-tile condition) (tiles condition)))))

(define-condition invalid-shuntsu (riichi-evaluator-error)
  ((%offending-tile :reader offending-tile :initarg :offending-tile))
  (:default-initargs
   :offending-tile (a:required-argument :offending-tile))
  (:report
   (lambda (condition stream)
     (let* ((tile (offending-tile condition))
            (error-type (if (suited-p tile) "lowest" "honor")))
       (format stream "Attempted to make a shuntsu with ~A tile ~A."
               error-type tile)))))

(define-condition set-reader-error (riichi-evaluator-error)
  ((%offending-string :initarg :offending-string :accessor offending-string))
  (:report (lambda (condition stream)
             (format stream "Attempted to read an invalid set: ~S"
                     (offending-string condition)))))

(define-condition invalid-kokushi-musou (riichi-evaluator-error)
  ((%offending-tile :reader offending-tile :initarg :offending-tile))
  (:default-initargs
   :offending-tile (a:required-argument :offending-tile))
  (:report
   (lambda (condition stream)
     (let* ((tile (offending-tile condition)))
       (format stream "Attempted to make a kokushi musou with tile ~A."
               tile)))))

(define-condition singles-set-contains-duplicates (riichi-evaluator-error)
  ((%tiles :reader tiles :initarg :tiles))
  (:default-initargs
   :tiles (a:required-argument :tiles))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to make a singles set with tile list ~S, which ~
                     contains duplicate tiles."
             (tiles condition)))))

(define-condition twelve-singles-and-pair-set-contains-duplicates
    (riichi-evaluator-error)
  ((%pair-tile :reader pair-tile :initarg :pair-tile)
   (%single-tiles :reader single-tiles :initarg :single-tiles))
  (:default-initargs
   :pair-tile (a:required-argument :pair-tile)
   :single-tiles (a:required-argument :single-tiles))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to make a twelve-singles-and-pair set with tile ~
                     list ~S, which contains the pair tile ~S."
             (single-tiles condition) (pair-tile condition)))))

(define-condition invalid-puuta (riichi-evaluator-error)
  ((%offending-tiles :reader offending-tiles :initarg :offending-tiles))
  (:default-initargs
   :offending-tiles (a:required-argument :offending-tiles))
  (:report
   (lambda (condition stream)
     (destructuring-bind (tile-1 tile-2) (offending-tiles condition)
       (format stream "Attempted to make a puuta with neighboring tiles ~A and ~
                      ~A." tile-1 tile-2)))))

(define-condition full-hand-set-invalid-tile-count (riichi-evaluator-error)
  ((%tiles :reader tiles :initarg :tiles)
   (%expected-tile-count :reader expected-tile-count
                         :initarg :expected-tile-count))
  (:default-initargs
   :tiles (a:required-argument :tiles)
   :expected-tile-count (a:required-argument :expected-tile-count))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to make a full hand set with tile list ~A, ~
                     which contains fewer than ~D tiles expected for that set."
             (tiles condition) (expected-tile-count condition)))))

;;; Protocol

(p:define-protocol-class set () ())

(defgeneric set= (set-1 set-2)
  (:method (set-1 set-2) nil))

(defgeneric set-tile-count (set))

(defun print-set (set &optional (stream t))
  (case stream
    ((t) (print-set-using-class set *standard-output*))
    ((nil) (with-output-to-string (stream) (print-set-using-class set stream)))
    (t (print-set-using-class set stream))))

(defgeneric print-set-using-class (set stream)
  (:method :around (set stream) (call-next-method) set))

(defmethod print-object ((set set) stream)
  (print-unreadable-object (set stream :type nil :identity nil)
    (format stream "~A " (type-of set))
    (print-set set stream)))

(p:define-protocol-class same-tile-set (set)
  ((%tile :reader same-tile-set-tile :initarg :tile))
  (:default-initargs
   :tile (a:required-argument :same-tile-set-tile)))

;;; TODO: do typechecks in :BEFORE methods rather than :AFTER methods.
(defmethod initialize-instance :after ((set same-tile-set) &key)
  (let ((tile (same-tile-set-tile set)))
    (unless (tile-p tile)
      (error 'invalid-set-element :datum tile :expected-type 'tile))))

(defmethod set= ((set-1 same-tile-set) (set-2 same-tile-set))
  (and (eq (class-of set-1) (class-of set-2))
       (tile= (same-tile-set-tile set-1) (same-tile-set-tile set-2))))

(defmethod tiles ((set same-tile-set))
  (make-list (set-tile-count set) :initial-element (same-tile-set-tile set)))

(p:define-protocol-class closed-set (set) ())

(p:define-protocol-class open-set (set)
  ((%taken-from :reader taken-from :initarg :taken-from))
  (:default-initargs
   :taken-from (a:required-argument :taken-from)))

(defmethod initialize-instance :after ((set open-set) &key)
  (let ((taken-from (taken-from set)))
    (unless (member taken-from *other-players*)
      (error 'invalid-tile-taken-from
             :datum taken-from
             :expected-type '#.`(member ,*other-players*)))))

(defun print-open-set (set stream if-kamicha if-toimen if-shimocha)
  (let* ((rank (rank (first (tiles set))))
         (tiles (subst rank :rank (case (taken-from set)
                                    (:kamicha if-kamicha)
                                    (:toimen if-toimen)
                                    (:shimocha if-shimocha)))))
    (format stream "~{~A~}" tiles)))

(p:define-protocol-class standard-set (set) ())

(p:define-protocol-class toitsu (same-tile-set) ())
(p:define-protocol-class koutsu (same-tile-set standard-set) ())
(p:define-protocol-class kantsu (same-tile-set standard-set) ())

(defmethod set-tile-count ((set toitsu)) (values 2 0))
(defmethod set-tile-count ((set koutsu)) (values 3 0))
(defmethod set-tile-count ((set kantsu)) (values 4 1))

(p:define-protocol-class shuntsu (standard-set)
  ((%lowest-tile :reader shuntsu-lowest-tile :initarg :lowest-tile))
  (:default-initargs
   :lowest-tile (a:required-argument :lowest-tile)))

(defmethod initialize-instance :after ((set shuntsu) &key)
  (let ((tile (shuntsu-lowest-tile set)))
    (unless (tile-p tile)
      (error 'invalid-set-element :datum tile :expected-type 'tile))
    (unless (and (suited-p tile) (<= (rank tile) 7))
      (error 'invalid-shuntsu :offending-tile tile))))

(defmethod tiles ((set shuntsu))
  (let* ((tile-1 (shuntsu-lowest-tile set))
         (suit (suit tile-1)) (rank (rank tile-1))
         (tile-2 (make-instance 'suited-tile :suit suit :rank (+ 1 rank)))
         (tile-3 (make-instance 'suited-tile :suit suit :rank (+ 2 rank))))
    (list tile-1 tile-2 tile-3)))

(defmethod set= ((set-1 shuntsu) (set-2 shuntsu))
  (and (eq (class-of set-1) (class-of set-2))
       (tile= (shuntsu-lowest-tile set-1) (shuntsu-lowest-tile set-2))))

(defmethod set-tile-count ((set shuntsu)) (values 3 0))

(p:define-protocol-class full-hand-set (set) ())

(defmethod initialize-instance :after ((set full-hand-set) &key)
  (let ((expected-tile-count (set-tile-count set))
        (tiles (tiles set)))
    (unless (= expected-tile-count (length tiles))
      (error 'full-hand-set-invalid-tile-count
             :tiles tiles
             :expected-tile-count expected-tile-count))))

(defmethod set-tile-count ((set full-hand-set)) (values 14 0))

(p:define-protocol-class singles-set () ())

(defgeneric single-tiles (singles-set))

(defmethod initialize-instance :after ((set singles-set) &key)
  (let ((single-tiles (single-tiles set)))
    (unless (= (length single-tiles)
               (length (remove-duplicates single-tiles :test #'tile=)))
      (error 'singles-set-contains-duplicates :tiles single-tiles))))

(defmethod set= ((set-1 singles-set) (set-2 singles-set))
  (and (eq (class-of set-1) (class-of set-2))
       (tile-list= (tiles set-1) (tiles set-2))))

(p:define-protocol-class twelve-singles-and-pair-set (singles-set full-hand-set)
  ((%pair-tile :reader pair-tile :initarg :pair-tile))
  (:default-initargs
   :pair-tile (a:required-argument :pair-tile)))

(defmethod initialize-instance :before
    ((set twelve-singles-and-pair-set) &key pair-tile)
  (unless (tile-p pair-tile)
    (error 'invalid-set-element :datum pair-tile :expected-type 'tile)))

(defmethod initialize-instance :after ((set twelve-singles-and-pair-set) &key)
  (let ((single-tiles (single-tiles set))
        (pair-tile (pair-tile set)))
    (when (member pair-tile single-tiles :test #'tile=)
      (error 'twelve-singles-and-pair-set-contains-duplicates
             :pair-tile pair-tile :single-tiles single-tiles))))

(p:define-protocol-class kokushi-musou (twelve-singles-and-pair-set) ())

(defmethod initialize-instance :after ((set kokushi-musou) &key)
  (let ((pair-tile (pair-tile set)))
    (unless (or (terminal-p pair-tile) (honor-p pair-tile))
      (error 'invalid-kokushi-musou :offending-tile pair-tile))))

(defparameter *kokushi-musou-tiles*
  '([1m] [9m] [1p] [9p] [1s] [9s] [EW] [SW] [WW] [NW] [WD] [GD] [RD]))

(defmethod single-tiles ((set kokushi-musou))
  (remove (pair-tile set) *kokushi-musou-tiles* :test #'tile=))

(defmethod tiles ((set kokushi-musou))
  (sort (cons (pair-tile set) (copy-list *kokushi-musou-tiles*)) #'tile<))

(defmethod set= ((set-1 kokushi-musou) (set-2 kokushi-musou))
  (and (eq (class-of set-1) (class-of set-2))
       (tile= (pair-tile set-1) (pair-tile set-2))))

(p:define-protocol-class fourteen-singles-set (singles-set full-hand-set)
  ((%single-tiles :reader single-tiles :initarg :single-tiles))
  (:default-initargs
   :single-tiles (a:required-argument :single-tiles)))

(p:define-protocol-class puuta (singles-set) ())

(defun verify-puuta-tiles (tiles)
  (dolist (tile-1 tiles)
    (unless (honor-p tile-1)
      (dolist (tile-2 (remove tile-1 tiles :count 1 :test #'tile=))
        (unless (honor-p tile-2)
          (when (eq (suit tile-1) (suit tile-2))
            (let ((rank-difference (abs (- (rank tile-1) (rank tile-2)))))
              (when (< rank-difference 3)
                (return-from verify-puuta-tiles (list tile-1 tile-2))))))))))

(defclass shiisan-puuta (twelve-singles-and-pair-set closed-set puuta)
  ((%single-tiles :reader single-tiles :initarg :single-tiles))
  (:default-initargs
   :single-tiles (a:required-argument :single-tiles)))

(defmethod initialize-instance :after ((set shiisan-puuta) &key)
  (a:when-let ((offending-tiles (verify-puuta-tiles (cons (pair-tile set)
                                                          (single-tiles set)))))
    (error 'invalid-puuta :offending-tiles offending-tiles)))

(defmethod tiles ((set shiisan-puuta))
  (sort (list* (pair-tile set) (pair-tile set) (copy-list (single-tiles set)))
        #'tile<))

(defclass shiisuu-puuta (fourteen-singles-set closed-set puuta) ())

(defmethod initialize-instance :after ((set shiisuu-puuta) &key)
  (a:when-let ((offending-tiles (verify-puuta-tiles (single-tiles set))))
    (error 'invalid-puuta :offending-tiles offending-tiles)))

(defmethod tiles ((set shiisuu-puuta))
  (single-tiles set))

;;; Concrete classes

(defclass antoi (toitsu closed-set) ())
(defun antoi (tile)
  (make-instance 'antoi :tile tile))

(defclass mintoi (toitsu open-set) ())
(defun mintoi (tile taken-from)
  (make-instance 'mintoi :tile tile :taken-from taken-from))

(defclass ankou (koutsu closed-set) ())
(defun ankou (tile)
  (make-instance 'ankou :tile tile))

(defclass minkou (koutsu open-set) ())
(defun minkou (tile taken-from)
  (make-instance 'minkou :tile tile :taken-from taken-from))

(defclass ankan (kantsu closed-set) ())
(defun ankan (tile)
  (make-instance 'ankan :tile tile))

(defclass daiminkan (kantsu open-set) ())
(defun daiminkan (tile taken-from)
  (make-instance 'daiminkan :tile tile :taken-from taken-from))

(defclass shouminkan (kantsu open-set) ())
(defun shouminkan (tile taken-from)
  (make-instance 'shouminkan :tile tile :taken-from taken-from))

(defclass anjun (shuntsu closed-set) ())
(defun anjun (lowest-tile)
  (make-instance 'anjun :lowest-tile lowest-tile))

(defclass minjun (shuntsu open-set)
  ((%open-tile :reader open-tile :initarg :open-tile))
  (:default-initargs
   :taken-from :kamicha
   :open-tile (a:required-argument :open-tile)))
(defun minjun (lowest-tile open-tile taken-from)
  (make-instance 'minjun :lowest-tile lowest-tile
                         :open-tile open-tile :taken-from taken-from))

(defmethod initialize-instance :before ((set minjun) &key open-tile taken-from)
  (unless (tile-p open-tile)
    (error 'invalid-set-element :datum open-tile :expected-type 'tile))
  (unless (member taken-from *other-players*)
    (error 'invalid-tile-taken-from
           :datum taken-from
           :expected-type '#.`(member ,*other-players*))))

(defmethod initialize-instance :after ((set minjun) &key)
  (let ((tile (open-tile set))
        (tiles (tiles set)))
    (unless (member tile tiles :test #'tile=)
      (error 'open-tile-not-in-set :open-tile tile :tiles tiles))))

(defmethod set= ((set-1 minjun) (set-2 minjun))
  (and (tile= (open-tile set-1) (open-tile set-2))
       (call-next-method)))

(defclass closed-kokushi-musou (kokushi-musou closed-set) ())
(defclass open-kokushi-musou (kokushi-musou open-set) ())
(defmethod set= ((set-1 open-kokushi-musou) (set-2 open-kokushi-musou))
  (and (tile= (open-tile set-1) (open-tile set-2))
       (call-next-method)))

;;; Set printer

(defmethod print-set-using-class ((set closed-set) stream)
  (print-tile-list (tiles set) stream))

(defmethod print-set-using-class :after ((set open-set) stream)
  (let* ((tile (first (tiles set)))
         (suit (suit tile)))
    (princ (a:assoc-value *print-table* suit) stream)))

(defmethod print-set-using-class ((set mintoi) stream)
  (let ((rank (rank (same-tile-set-tile set))))
    (print-open-set set stream
                    (list rank "*" rank)
                    (list rank "*" rank "*")
                    (list rank rank "*"))))

(defmethod print-set-using-class ((set minkou) stream)
  (let ((rank (rank (same-tile-set-tile set))))
    (print-open-set set stream
                    (list rank "*" rank rank)
                    (list rank rank "*" rank)
                    (list rank rank rank "*"))))

(defmethod print-set-using-class ((set daiminkan) stream)
  (let ((rank (rank (same-tile-set-tile set))))
    (print-open-set set stream
                    (list rank "*" rank rank rank)
                    (list rank rank "*" rank rank)
                    (list rank rank rank rank "*"))))

(defmethod print-set-using-class ((set shouminkan) stream)
  (let ((rank (rank (same-tile-set-tile set))))
    (print-open-set set stream
                    (list rank "*" rank "**" rank rank)
                    (list rank rank "*" rank "**" rank)
                    (list rank rank rank "*" rank "**"))))

(defmethod print-set-using-class ((set minjun) stream)
  (let* ((tiles (tiles set))
         (open-tile (open-tile set))
         (remaining-tiles (remove open-tile tiles :test #'tile=))
         (ranks (mapcar #'rank (cons open-tile remaining-tiles))))
    (destructuring-bind (rank-1 rank-2 rank-3) ranks
      (print-open-set set stream
                      (list rank-1 "*" rank-2 rank-3)
                      (list rank-2 rank-1 "*" rank-3)
                      (list rank-2 rank-3 rank-1 "*")))))

;;; Set reader

(defun try-read-make-tile (rank suit)
  (if (eq suit :honor)
      (make-instance 'honor-tile :kind (nth (1- rank) *honors*))
      (make-instance 'suited-tile :suit suit :rank rank)))

(defun read-set (stream)
  (let ((string (loop for char = (peek-char t stream nil :eof t)
                      while (or (alphanumericp char) (eql char #\*))
                      collect char)))
    (read-set-from-string string)))

(defun read-set-from-string (string)
  (flet ((complain () (error 'set-reader-error :offending-string string)))
    (handler-case (let ((ordered-tiles (parse-set-string string)))
                    (or (try-read-set ordered-tiles) (complain)))
      (riichi-evaluator-error () (complain)))))

(defun parse-set-string (string)
  (prog ((stack '())
         (result '())
         (char nil)
         (i 0)
         (length (length string)))
   :start
     (when (= i length) (go :end))
   :pop-char
     (setf char (char string i))
     (cond ((char<= #\0 char #\9) (go :number-char))
           ((alpha-char-p char) (go :alpha-char))
           ((char= #\* char) (go :flip-last-number))
           (t (go :error)))
   :number-char
     (let ((rank (digit-char-p char)))
       (push (list rank nil) stack))
     (go :loop)
   :alpha-char
     (dolist (elt stack)
       (destructuring-bind (number state) elt
         (let ((suit (a:rassoc-value *print-table* char
                                     :test #'char-equal)))
           (unless suit (go :error))
           (push (list number suit state) result))))
     (setf stack '())
     (go :loop)
   :flip-last-number
     (destructuring-bind (rank state) (pop stack)
       (let ((new-state (ecase state
                          ((nil) :flip)
                          (:flip :shouminkan))))
         (push (list rank new-state) stack)))
     (go :loop)
   :loop
     (incf i)
     (go :start)
   :end
     (return result)
   :error
     (error 'riichi-evaluator-error)))

(defgeneric try-read-set (ordered)
  (:method-combination chained-or))

(defmethod try-read-set :toitsu (ordered)
  (when (= 2 (length ordered))
    (destructuring-bind ((rank-1 suit-1 state-1)
                         (rank-2 suit-2 state-2))
        ordered
      (when (and (= rank-1 rank-2)
                 (eq suit-1 suit-2))
        (let ((tile (try-read-make-tile rank-1 suit-1)))
          (a:switch ((list state-1 state-2) :test #'equal)
            ('(nil nil) (antoi tile))
            ('(:flip nil) (mintoi tile :kamicha))
            ('(:flip :flip) (mintoi tile :toimen))
            ('(nil :flip) (mintoi tile :shimocha))))))))

(defmethod try-read-set :koutsu (ordered)
  (when (= 3 (length ordered))
    (destructuring-bind ((rank-1 suit-1 state-1)
                         (rank-2 suit-2 state-2)
                         (rank-3 suit-3 state-3))
        ordered
      (when (and (= rank-1 rank-2 rank-3)
                 (eq suit-1 suit-2) (eq suit-1 suit-3))
        (let ((tile (try-read-make-tile rank-1 suit-1)))
          (a:switch ((list state-1 state-2 state-3) :test #'equal)
            ('(nil nil nil) (ankou tile))
            ('(:flip nil nil) (minkou tile :kamicha))
            ('(nil :flip nil) (minkou tile :toimen))
            ('(nil nil :flip) (minkou tile :shimocha))))))))

(defmethod try-read-set :kantsu (ordered)
  (when (= 4 (length ordered))
    (destructuring-bind ((rank-1 suit-1 state-1)
                         (rank-2 suit-2 state-2)
                         (rank-3 suit-3 state-3)
                         (rank-4 suit-4 state-4))
        ordered
      (when (and (= rank-1 rank-2 rank-3 rank-4)
                 (eq suit-1 suit-2) (eq suit-1 suit-3) (eq suit-1 suit-4))
        (let ((tile (try-read-make-tile rank-1 suit-1)))
          (a:switch ((list state-1 state-2 state-3 state-4) :test #'equal)
            ('(nil nil nil nil) (ankan tile))
            ('(:flip nil nil nil) (daiminkan tile :kamicha))
            ('(nil :flip nil nil) (daiminkan tile :toimen))
            ('(nil nil nil :flip) (daiminkan tile :shimocha))
            ('(:flip :shouminkan nil nil) (shouminkan tile :kamicha))
            ('(nil :flip :shouminkan nil) (shouminkan tile :toimen))
            ('(nil nil :flip :shouminkan) (shouminkan tile :shimocha))))))))

(defun shuntsu-ranks-p (rank-1 rank-2 rank-3)
  (and (<= 1 rank-1 9) (<= 1 rank-2 9) (<= 1 rank-3 9)
       (or (= rank-1 (+ 1 rank-2) (+ 2 rank-3))
           (= rank-1 (+ 1 rank-3) (+ 2 rank-2))
           (= rank-2 (+ 1 rank-1) (+ 2 rank-3))
           (= rank-2 (+ 1 rank-3) (+ 2 rank-1))
           (= rank-3 (+ 1 rank-1) (+ 2 rank-2))
           (= rank-3 (+ 1 rank-2) (+ 2 rank-1)))))

(defmethod try-read-set :shuntsu (ordered)
  (when (= 3 (length ordered))
    (destructuring-bind ((rank-1 suit-1 state-1)
                         (rank-2 suit-2 state-2)
                         (rank-3 suit-3 state-3))
        ordered
      (declare (ignore state-1 state-2 state-3))
      (when (and (eq suit-1 suit-2) (eq suit-1 suit-3)
                 (shuntsu-ranks-p rank-1 rank-2 rank-3))
        (let* ((lowest-rank (min rank-1 rank-2 rank-3))
               (lowest-tile (try-read-make-tile lowest-rank suit-1)))
          (if (member :flip ordered :key #'third)
              (when (and (= 1 (count :flip ordered :key #'third))
                         (= 2 (count nil ordered :key #'third)))
                (let* ((open-position (position :flip ordered :key #'third))
                       (open-rank (first (nth open-position ordered)))
                       (open-tile (try-read-make-tile open-rank suit-1))
                       (taken-from (ecase open-position
                                     (0 :kamicha)
                                     (1 :toimen)
                                     (2 :shimocha))))
                  (minjun lowest-tile open-tile taken-from)))
              (anjun lowest-tile)))))))

;;; Tile-set matcher

(defun try-make-shuntsu (tiles tile forbidden-sets class tile-count args)
  (declare (ignore tile-count))
  (let (set)
    (flet ((try-find-set (t1 t2 t3)
             (and t1 t2 t3
                  (setf set (apply class t1 args))
                  (not (member set forbidden-sets :test #'set=))))
           (make-pred (n)
             (lambda (x) (and (suited-p x) (eq (suit tile) (suit x))
                              (= (+ (rank tile) n) (rank x))))))
      (let* ((2- (find-if (make-pred -2) tiles))
             (1- (find-if (make-pred -1) tiles))
             (1+ (find-if (make-pred 1) tiles))
             (2+ (find-if (make-pred 2) tiles)))
        (cond ((try-find-set 2- 1- tile)
               (list set (bag-difference tiles (list 2- 1- tile)
                                         :test #'tile=)))
              ((try-find-set 1- tile 1+)
               (list set (bag-difference tiles (list 1- tile 1+)
                                         :test #'tile=)))
              ((try-find-set tile 1+ 2+)
               (list set (bag-difference tiles (list tile 1+ 2+)
                                         :test #'tile=))))))))

(defun try-make-same-tile-set (tiles tile forbidden-sets class tile-count args)
  (when (<= tile-count (count tile tiles :test #'tile=))
    (let ((set (apply class tile args)))
      (unless (member set forbidden-sets :test #'set=)
        (list set (remove tile tiles :count tile-count :test #'tile=))))))

(defun try-make-set
    (tiles winning-tile consume-winning-tile-p forbidden-sets class tile-count
     make-fn &rest args)
  (multiple-value-or
    (values-list
     (cond
       ((not consume-winning-tile-p)
        (dolist (tile tiles)
          (a:when-let ((result (funcall make-fn tiles tile forbidden-sets
                                        class tile-count args)))
            (return (append result (list winning-tile))))))
       (winning-tile
        (a:when-let ((result (funcall make-fn
                                      (cons winning-tile tiles) winning-tile
                                      forbidden-sets class tile-count args)))
          (append result (list nil))))
       (t
        (a:when-let ((result (funcall make-fn tiles winning-tile
                                      forbidden-sets class tile-count args)))
          (append result (list nil))))))
    (values nil tiles winning-tile)))

(defgeneric try-make-set-from-tiles (tiles winning-tile win-from forbidden-sets)
  (:method-combination chained-or))

(macrolet
    ((make ((class kind) &body body)
       `(defmethod try-make-set-from-tiles chained-or ,class ,kind
          (tiles winning-tile win-from forbidden-sets)
          ,@body))
     (make-set-maker-free-tiles-only (class count make-fn)
       `(make (,class :free-tiles-only)
              (try-make-set tiles winning-tile nil
                            forbidden-sets ',class ,count
                            ,make-fn)))
     (make-set-maker-winning-tile-tsumo (class count make-fn)
       `(make (,class :winning-tile-tsumo)
              (if (and winning-tile (eq win-from :tsumo))
                  (try-make-set tiles winning-tile t
                                forbidden-sets ',class ,count
                                ,make-fn)
                  (values nil tiles winning-tile))))
     (make-set-maker-winning-tile-ron (class count make-fn &rest args)
       `(make (,class :winning-tile-ron)
              (if (and winning-tile (not (eq win-from :tsumo)))
                  (try-make-set tiles winning-tile t
                                forbidden-sets ',class ,count
                                ,make-fn
                                ,@args)
                  (values nil tiles winning-tile)))))
  (make-set-maker-free-tiles-only antoi 2 #'try-make-same-tile-set)
  (make-set-maker-free-tiles-only ankou 3 #'try-make-same-tile-set)
  (make-set-maker-free-tiles-only anjun 3 #'try-make-shuntsu)
  (make-set-maker-winning-tile-tsumo antoi 2 #'try-make-same-tile-set)
  (make-set-maker-winning-tile-tsumo ankou 3 #'try-make-same-tile-set)
  (make-set-maker-winning-tile-tsumo anjun 3 #'try-make-shuntsu)
  (make-set-maker-winning-tile-ron mintoi 2 #'try-make-same-tile-set
                                   win-from)
  (make-set-maker-winning-tile-ron minkou 3 #'try-make-same-tile-set
                                   win-from)
  (make-set-maker-winning-tile-ron minjun 3 #'try-make-shuntsu
                                   winning-tile win-from))
