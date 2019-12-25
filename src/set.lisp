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
   #:invalid-shuntsu #:minjun-invalid-meld
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

(define-condition minjun-invalid-meld (riichi-evaluator-error)
  ((%taken-from :reader taken-from :initarg :taken-from)
   (%set :reader set :initarg :set))
  (:default-initargs
   :tiles (a:required-argument :tiles)
   :set (a:required-argument :set))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to make a minjun ~A with tile taken from ~A ~
                     instead of kami-cha."
             (set condition) (taken-from condition)))))

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
(defclass mintoi (toitsu open-set) ())

(defclass ankou (koutsu closed-set) ())
(defclass minkou (koutsu open-set) ())

(defclass ankan (kantsu closed-set) ())
(defclass daiminkan (kantsu open-set) ())
(defclass shouminkan (kantsu open-set) ())

(defclass anjun (shuntsu closed-set) ())
(defclass minjun (shuntsu open-set)
  ((%open-tile :reader open-tile :initarg :open-tile))
  (:default-initargs
   :taken-from :kamicha ;; TODO test this default initarg
   :open-tile (a:required-argument :open-tile)))

(defmethod initialize-instance :after ((set minjun) &key)
  (let ((tile (open-tile set)))
    (unless (tile-p tile)
      (error 'invalid-set-element :datum tile :expected-type 'tile))
    (let ((tiles (tiles set)))
      (unless (member tile tiles :test #'tile=)
        (error 'open-tile-not-in-set :open-tile tile :tiles tiles))))
  (let ((taken-from (taken-from set)))
    (unless (eq :kamicha taken-from)
      (error 'minjun-invalid-meld :taken-from taken-from :set set))))

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
                      (list rank-1 rank-2 "*" rank-3)
                      (list rank-1 rank-2 rank-3 "*")))))

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
        (let ((tile (try-read-make-tile rank-1 suit)))
          (make-instance 'antoi :tile tile))))))

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
        (let ((tile (try-read-make-tile rank-1 suit)))
          (make-instance 'mintoi :tile tile
                                 :taken-from taken-from))))))

(defmethod try-read-set chained-or :mintoi-toimen ((string string))
  (destructure-string (c1 c2 c3 c4 c5) string
    (when (char= #\* c2 c4)
      (a:when-let ((rank-1 (digit-char-p c1))
                   (rank-2 (digit-char-p c3))
                   (suit (a:rassoc-value *print-table* c5)))
        (when (= rank-1 rank-2)
          (let ((tile (try-read-make-tile rank-1 suit)))
            (make-instance 'mintoi :tile tile
                                   :taken-from :toimen)))))))

(defmethod try-read-set chained-or :ankou ((string string))
  (destructure-string (c1 c2 c3 c4) string
    (a:when-let ((rank-1 (digit-char-p c1))
                 (rank-2 (digit-char-p c2))
                 (rank-3 (digit-char-p c3))
                 (suit (a:rassoc-value *print-table* c4)))
      (when (= rank-1 rank-2 rank-3)
        (let ((tile (try-read-make-tile rank-1 suit)))
          (make-instance 'ankou :tile tile))))))

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
        (let ((tile (try-read-make-tile rank-1 suit)))
          (make-instance 'minkou :tile tile
                                 :taken-from taken-from))))))

(defmethod try-read-set chained-or :ankan ((string string))
  (destructure-string (c1 c2 c3 c4 c5) string
    (a:when-let ((rank-1 (digit-char-p c1))
                 (rank-2 (digit-char-p c2))
                 (rank-3 (digit-char-p c3))
                 (rank-4 (digit-char-p c4))
                 (suit (a:rassoc-value *print-table* c5)))
      (when (= rank-1 rank-2 rank-3 rank-4)
        (let ((tile (try-read-make-tile rank-1 suit)))
          (make-instance 'ankan :tile tile))))))

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
        (let ((tile (try-read-make-tile rank-1 suit)))
          (make-instance 'daiminkan :tile tile
                                    :taken-from taken-from))))))

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
        (let ((tile (try-read-make-tile rank-1 suit)))
          (make-instance 'shouminkan :tile tile
                                     :taken-from taken-from))))))

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
              (let ((tile (make-instance 'suited-tile
                                         :suit suit
                                         :rank (min rank-1 rank-2 rank-3))))
                (make-instance 'anjun :lowest-tile tile)))))))))

(defmethod try-read-set chained-or :minjun ((string string))
  (destructure-string (c1 c2 c3 c4 c5) string
    (when (char= #\* c2)
      (a:when-let ((rank-1 (digit-char-p c1))
                   (rank-2 (digit-char-p c3))
                   (rank-3 (digit-char-p c4))
                   (suit (a:rassoc-value *print-table* c5)))
        (unless (eq suit :honor)
          (let ((tiles (mapcar (a:curry #'make-instance 'suited-tile
                                        :suit suit :rank)
                               (list rank-1 rank-2 rank-3))))
            (destructuring-bind (tile-1 tile-2 tile-3) (sort tiles #'tile<)
              (when (and (tile-consec-p tile-1 tile-2)
                         (tile-consec-p tile-2 tile-3))
                (let ((open-tile (make-instance 'suited-tile
                                                :suit suit
                                                :rank rank-1)))
                  (make-instance 'minjun :lowest-tile tile-1
                                         :open-tile open-tile
                                         :taken-from :kamicha))))))))))

;;; Tile-set matcher

;; TODO test this
(defgeneric try-make-set-from-tiles (tiles winning-tile tsumop forbidden-sets)
  (:method-combination chained-or))

(defun try-make-same-tile-set-from-tiles-with-winning-tile
    (tiles winning-tile forbidden-sets class tile-count &rest args)
  (multiple-value-or
   (when (<= (1- tile-count) (count winning-tile tiles :test #'tile=))
     (let ((set (apply #'make-instance class :tile winning-tile args)))
       (unless (member set forbidden-sets :test #'set=)
         (values set nil
                 (remove winning-tile tiles :count (1- tile-count)
                                            :test #'tile=)))))
   (values nil nil nil)))

(defun try-make-same-tile-set-from-tiles-without-winning-tile
    (tiles winning-tile forbidden-sets class tile-count &rest args)
  (multiple-value-or
   (dolist (tile tiles)
     (when (<= tile-count (count tile tiles :test #'tile=))
       (let ((set (apply #'make-instance class :tile tile args)))
         (unless (member set forbidden-sets :test #'set=)
           (return (values set winning-tile
                           (remove tile tiles :count tile-count
                                              :test #'tile=)))))))
   (values nil nil nil)))

(macrolet
    ((make ((class format-control) &body body)
       (let ((name (a:format-symbol :keyword format-control class)))
         `(defmethod try-make-set-from-tiles chained-or ,name
            (tiles winning-tile taken-from forbidden-sets)
            ,@body)))
     (define-set-maker-min (class count)
       `(make (class "~A-WINNING-TILE")
              (when (not (eq taken-from :tsumo))
                (try-make-same-tile-set-from-tiles-with-winning-tile
                 tiles winning-tile forbidden-sets ',class ,count
                 :taken-from taken-from))))
     (define-set-maker-an-agaripai (class count)
       `(make (class "~A-WINNING-TILE")
              (when (eq taken-from :tsumo)
                (try-make-same-tile-set-from-tiles-with-winning-tile
                 tiles winning-tile forbidden-sets ',class ,count))))
     (define-set-maker-an-no-agaripai (class count)
       `(make (class "~A-NO-WINNING-TILE")
              (try-make-same-tile-set-from-tiles-without-winning-tile
               tiles winning-tile forbidden-sets ',class ,count))))
  (define-set-maker-min mintoi 2)
  (define-set-maker-min minkou 3)
  (define-set-maker-min minkan 4)
  (define-set-maker-an-agaripai antoi 2)
  (define-set-maker-an-agaripai ankou 3)
  (define-set-maker-an-agaripai ankan 4)
  (define-set-maker-an-no-agaripai antoi 2)
  (define-set-maker-an-no-agaripai ankou 3)
  (define-set-maker-an-no-agaripai ankan 4))

;; (defmethod try-make-set-from-tiles chained-or :antoi-no-winning-tile
;;     (tiles winning-tile taken-from forbidden-sets)
;;   (try-make-same-tile-set-from-tiles-without-winning-tile
;;    tiles winning-tile forbidden-sets 'antoi 2))

;; (defmethod try-make-set-from-tiles chained-or :ankou-no-winning-tile
;;     (tiles winning-tile taken-from forbidden-sets)
;;   (try-make-same-tile-set-from-tiles-without-winning-tile
;;    tiles winning-tile forbidden-sets 'ankou 3))

;; (defmethod try-make-set-from-tiles chained-or :ankan-no-winning-tile
;;     (tiles winning-tile taken-from forbidden-sets)
;;   (try-make-same-tile-set-from-tiles-without-winning-tile
;;    tiles winning-tile forbidden-sets 'ankan 4))

;; (defmethod try-make-set-from-tiles chained-or :mintoi-winning-tile
;;     (tiles winning-tile taken-from forbidden-sets)
;;   (when (not (eq taken-from :tsumo))
;;     (try-make-same-tile-set-from-tiles-with-winning-tile
;;      tiles winning-tile forbidden-sets 'mintoi 2 :taken-from taken-from)))

;; (defmethod try-make-set-from-tiles chained-or :minkou-winning-tile
;;     (tiles winning-tile taken-from forbidden-sets)
;;   (when (not (eq taken-from :tsumo))
;;     (try-make-same-tile-set-from-tiles-with-winning-tile
;;      tiles winning-tile forbidden-sets 'minkou 3 :taken-from taken-from)))

;; (defmethod try-make-set-from-tiles chained-or :minkan-winning-tile
;;     (tiles winning-tile taken-from forbidden-sets)
;;   (when (not (eq taken-from :tsumo))
;;     (try-make-same-tile-set-from-tiles-with-winning-tile
;;      tiles winning-tile forbidden-sets 'minkan 4 :taken-from taken-from)))

;; (defmethod try-make-set-from-tiles chained-or :antoi-winning-tile
;;     (tiles winning-tile taken-from forbidden-sets)
;;   (when (eq taken-from :tsumo)
;;     (try-make-same-tile-set-from-tiles-with-winning-tile
;;      tiles winning-tile forbidden-sets 'antoi 2)))

;; (defmethod try-make-set-from-tiles chained-or :ankou-winning-tile
;;     (tiles winning-tile taken-from forbidden-sets)
;;   (when (eq taken-from :tsumo)
;;     (try-make-same-tile-set-from-tiles-with-winning-tile
;;      tiles winning-tile forbidden-sets 'ankou 3)))

;; (defmethod try-make-set-from-tiles chained-or :ankan-winning-tile
;;     (tiles winning-tile taken-from forbidden-sets)
;;   (when (eq taken-from :tsumo)
;;     (try-make-same-tile-set-from-tiles-with-winning-tile
;;      tiles winning-tile forbidden-sets 'ankan 4)))
