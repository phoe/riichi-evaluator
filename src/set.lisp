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
                    (#:p #:protest/base)
                    (#:m #:closer-mop))
  (:shadow #:set)
  (:export
   ;; Conditions
   #:invalid-set-element #:invalid-tile-taken-from #:open-tile-not-in-set
   #:invalid-shuntsu
   #:set-reader-error #:offending-string
   ;; Condition accessors
   #:open-tile #:tiles #:taken-from
   ;; Protocol
   #:set #:set-count #:tiles #:set= #:set-tile-count
   #:same-tile-set #:closed-set #:open-set #:taken-from #:standard-set
   #:shuntsu #:toitsu #:koutsu #:kantsu
   ;; Concrete classes
   #:antoi #:mintoi #:anjun #:minjun #:ankou #:minkou
   #:ankan #:daiminkan #:shouminkan
   ;; Set reader and printer
   #:print-set #:read-set #:read-set-from-string
   ;; Tile-set matcher
   #:try-make-set-from-tiles
   ))

(in-package #:riichi-evaluator.set)

;;; Conditions

(define-condition invalid-set-element (type-error riichi-evaluator-error) ())

(define-condition invalid-tile-taken-from (type-error riichi-evaluator-error) ())

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

;;; Protocol

(p:define-protocol-class set ()
  ((%count :reader set-count :initarg %count))
  (:default-initargs %count (a:required-argument '%count)))

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

(defmethod initialize-instance :after ((set same-tile-set) &key)
  (let ((tile (same-tile-set-tile set)))
    (unless (tile-p tile)
      (error 'invalid-set-element :datum tile :expected-type 'tile))))

(defmethod set= ((set-1 same-tile-set) (set-2 same-tile-set))
  (and (eq (class-of set-1) (class-of set-2))
       (tile= (same-tile-set-tile set-1) (same-tile-set-tile set-2))))

(defmethod tiles ((set same-tile-set))
  (make-list (set-count set) :initial-element (same-tile-set-tile set)))

(p:define-protocol-class closed-set (set) ())

(defmethod print-set-using-class ((set closed-set) stream)
  (print-tile-list (tiles set) stream))

(p:define-protocol-class open-set (set)
  ((%taken-from :reader taken-from :initarg :taken-from))
  (:default-initargs
   :taken-from (a:required-argument :taken-from)))

(defmethod print-set-using-class :after ((set open-set) stream)
  (let* ((tile (first (tiles set)))
         (suit (suit tile)))
    (princ (a:assoc-value *print-table* suit) stream)))

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

(p:define-protocol-class toitsu (same-tile-set) ()
  (:default-initargs %count 2))
(p:define-protocol-class koutsu (same-tile-set standard-set) ()
  (:default-initargs %count 3))
(p:define-protocol-class kantsu (same-tile-set standard-set) ()
  (:default-initargs %count 4))

(defmethod set-tile-count ((set toitsu)) (values 2 0))
(defmethod set-tile-count ((set koutsu)) (values 3 0))
(defmethod set-tile-count ((set kantsu)) (values 4 1))

(p:define-protocol-class shuntsu (standard-set)
  ((%lowest-tile :reader shuntsu-lowest-tile :initarg :lowest-tile))
  (:default-initargs
   %count 3
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

(defmethod initialize-instance :after ((set minjun) &key)
  (let ((tile (open-tile set)))
    (unless (tile-p tile)
      (error 'invalid-set-element :datum tile :expected-type 'tile))
    (let ((tiles (tiles set)))
      (unless (member tile tiles :test #'tile=)
        (error 'open-tile-not-in-set :open-tile tile :tiles tiles))))
  (let ((taken-from (taken-from set)))
    (unless (member taken-from *other-players*)
      (error 'invalid-tile-taken-from
             :datum taken-from
             :expected-type '#.`(member ,*other-players*)))))

(defmethod set= ((set-1 minjun) (set-2 minjun))
  (and (eq (class-of set-1) (class-of set-2))
       (tile= (shuntsu-lowest-tile set-1) (shuntsu-lowest-tile set-2))
       (tile= (open-tile set-1) (open-tile set-2))))

;;; Set printer

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

(defun read-set (stream)
  (let ((string (loop for char = (peek-char t stream nil :eof t)
                      while (or (alphanumericp char) (eql char #\*))
                      collect char)))
    (read-set-from-string string)))

(defun read-set-from-string (string)
  (flet ((complain () (error 'set-reader-error :offending-string string)))
    (handler-case (or (try-read-set string) (complain))
      (riichi-evaluator-error () (complain)))))

(defun try-read-make-tile (rank suit)
  (if (eq suit :honor)
      (make-instance 'honor-tile :kind (nth (1- rank) *honors*))
      (make-instance 'suited-tile :suit suit :rank rank)))

(defmacro destructure-string (lambda-list string &body body)
  `(when (= ,(length lambda-list) (length ,string))
     (destructuring-bind ,lambda-list (coerce ,string 'list)
       ,@body)))

(defgeneric try-read-set (string)
  (:method-combination chained-or))

(defmethod try-read-set chained-or :antoi ((string string))
  (destructure-string (c1 c2 c3) string
    (a:when-let ((rank-1 (digit-char-p c1))
                 (rank-2 (digit-char-p c2))
                 (suit (a:rassoc-value *print-table* c3)))
      (when (= rank-1 rank-2)
        (antoi (try-read-make-tile rank-1 suit))))))

(defmethod try-read-set chained-or :mintoi-kami-shimo-cha ((string string))
  (destructure-string (c1 c2 c3 c4) string
    (a:when-let* ((taken-from (cond ((char= #\* c2) :kamicha)
                                    ((char= #\* c3) :shimocha)))
                  (rank-1 (digit-char-p c1))
                  (rank-2 (case taken-from
                            (:kamicha (digit-char-p c3))
                            (:shimocha (digit-char-p c2))))
                  (suit (a:rassoc-value *print-table* c4)))
      (when (= rank-1 rank-2)
        (mintoi (try-read-make-tile rank-1 suit) taken-from)))))

(defmethod try-read-set chained-or :mintoi-toimen ((string string))
  (destructure-string (c1 c2 c3 c4 c5) string
    (when (char= #\* c2 c4)
      (a:when-let ((rank-1 (digit-char-p c1))
                   (rank-2 (digit-char-p c3))
                   (suit (a:rassoc-value *print-table* c5)))
        (when (= rank-1 rank-2)
          (mintoi (try-read-make-tile rank-1 suit) :toimen))))))

(defmethod try-read-set chained-or :ankou ((string string))
  (destructure-string (c1 c2 c3 c4) string
    (a:when-let ((rank-1 (digit-char-p c1))
                 (rank-2 (digit-char-p c2))
                 (rank-3 (digit-char-p c3))
                 (suit (a:rassoc-value *print-table* c4)))
      (when (= rank-1 rank-2 rank-3)
        (ankou (try-read-make-tile rank-1 suit))))))

(defmethod try-read-set chained-or :minkou ((string string))
  (destructure-string (c1 c2 c3 c4 c5) string
    (a:when-let* ((taken-from (cond ((char= #\* c2) :kamicha)
                                    ((char= #\* c3) :toimen)
                                    ((char= #\* c4) :shimocha)))
                  (rank-1 (digit-char-p c1))
                  (rank-2 (case taken-from
                            (:kamicha (digit-char-p c3))
                            ((:toimen :shimocha) (digit-char-p c2))))
                  (rank-3 (case taken-from
                            ((:kamicha :toimen) (digit-char-p c4))
                            (:shimocha (digit-char-p c3))))
                  (suit (a:rassoc-value *print-table* c5)))
      (when (= rank-1 rank-2 rank-3)
        (minkou (try-read-make-tile rank-1 suit) taken-from)))))

(defmethod try-read-set chained-or :ankan ((string string))
  (destructure-string (c1 c2 c3 c4 c5) string
    (a:when-let ((rank-1 (digit-char-p c1))
                 (rank-2 (digit-char-p c2))
                 (rank-3 (digit-char-p c3))
                 (rank-4 (digit-char-p c4))
                 (suit (a:rassoc-value *print-table* c5)))
      (when (= rank-1 rank-2 rank-3 rank-4)
        (ankan (try-read-make-tile rank-1 suit))))))

(defmethod try-read-set chained-or :daiminkan ((string string))
  (destructure-string (c1 c2 c3 c4 c5 c6) string
    (a:when-let* ((taken-from (cond ((char= #\* c2) :kamicha)
                                    ((char= #\* c3) :toimen)
                                    ((char= #\* c5) :shimocha)))
                  (rank-1 (digit-char-p c1))
                  (rank-2 (case taken-from
                            (:kamicha (digit-char-p c3))
                            ((:toimen :shimocha) (digit-char-p c2))))
                  (rank-3 (case taken-from
                            ((:kamicha :toimen) (digit-char-p c4))
                            (:shimocha (digit-char-p c3))))
                  (rank-4 (case taken-from
                            ((:kamicha :toimen) (digit-char-p c5))
                            (:shimocha (digit-char-p c4))))
                  (suit (a:rassoc-value *print-table* c6)))
      (when (= rank-1 rank-2 rank-3 rank-4)
        (daiminkan (try-read-make-tile rank-1 suit) taken-from)))))

(defmethod try-read-set chained-or :shouminkan ((string string))
  (destructure-string (c1 c2 c3 c4 c5 c6 c7 c8) string
    (a:when-let* ((taken-from (cond ((char= #\* c2 c4 c5) :kamicha)
                                    ((char= #\* c3 c5 c6) :toimen)
                                    ((char= #\* c4 c6 c7) :shimocha)))
                  (rank-1 (digit-char-p c1))
                  (rank-2 (case taken-from
                            (:kamicha (digit-char-p c3))
                            ((:toimen :shimocha) (digit-char-p c2))))
                  (rank-3 (case taken-from
                            (:kamicha (digit-char-p c6))
                            (:toimen (digit-char-p c4))
                            (:shimocha (digit-char-p c3))))
                  (rank-4 (case taken-from
                            ((:kamicha :toimen) (digit-char-p c7))
                            (:shimocha (digit-char-p c5))))
                  (suit (a:rassoc-value *print-table* c8)))
      (when (= rank-1 rank-2 rank-3 rank-4)
        (shouminkan (try-read-make-tile rank-1 suit) taken-from)))))

(defmethod try-read-set chained-or :anjun ((string string))
  (destructure-string (c1 c2 c3 c4) string
    (a:when-let ((rank-1 (digit-char-p c1))
                 (rank-2 (digit-char-p c2))
                 (rank-3 (digit-char-p c3))
                 (suit (a:rassoc-value *print-table* c4)))
      (unless (eq suit :honor)
        (let ((tiles (mapcar (a:curry #'make-instance 'suited-tile
                                      :suit suit :rank)
                             (list rank-1 rank-2 rank-3))))
          (destructuring-bind (tile-1 tile-2 tile-3) (sort tiles #'tile<)
            (when (and (tile-consec-p tile-1 tile-2)
                       (tile-consec-p tile-2 tile-3))
              (let ((rank (min rank-1 rank-2 rank-3)))
                (anjun (make-instance 'suited-tile
                                      :suit suit :rank rank))))))))))

(defmethod try-read-set chained-or :minjun ((string string))
  (destructure-string (c1 c2 c3 c4 c5) string
    (a:when-let* ((taken-from (cond ((char= #\* c2) :kamicha)
                                    ((char= #\* c3) :toimen)
                                    ((char= #\* c4) :shimocha)))
                  (rank-1 (digit-char-p c1))
                  (rank-2 (case taken-from
                            (:kamicha (digit-char-p c3))
                            ((:toimen :shimocha) (digit-char-p c2))))
                  (rank-3 (case taken-from
                            ((:kamicha :toimen) (digit-char-p c4))
                            (:shimocha (digit-char-p c3))))
                  (suit (a:rassoc-value *print-table* c5)))
      (unless (eq suit :honor)
        (let ((tiles (mapcar (a:curry #'make-instance 'suited-tile
                                      :suit suit :rank)
                             (list rank-1 rank-2 rank-3))))
          (destructuring-bind (tile-1 tile-2 tile-3) (sort tiles #'tile<)
            (when (and (tile-consec-p tile-1 tile-2)
                       (tile-consec-p tile-2 tile-3))
              (let* ((open-rank (case taken-from
                                  (:kamicha rank-1)
                                  (:toimen rank-2)
                                  (:shimocha rank-3)))
                     (open-tile (make-instance 'suited-tile
                                               :suit suit
                                               :rank open-rank)))
                (minjun tile-1 open-tile taken-from)))))))))

;;; Tile-set matcher

(defun try-make-shuntsu (tiles tile forbidden-sets class tile-count args)
  (declare (ignore tile-count))
  (let (set)
    (flet ((tiles-found-p (t1 t2 t3)
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
        (cond ((tiles-found-p 2- 1- tile)
               (list set (bag-difference tiles (list 2- 1- tile)
                                         :test #'tile=)))
              ((tiles-found-p 1- tile 1+)
               (list set (bag-difference tiles (list 1- tile 1+)
                                         :test #'tile=)))
              ((tiles-found-p tile 1+ 2+)
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
