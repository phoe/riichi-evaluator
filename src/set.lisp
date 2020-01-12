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
                    (#:m #:closer-mop)
                    (#:mu #:moptilities))
  (:shadow #:set)
  (:export
   ;; Variables
   #:*kokushi-musou-tiles*
   ;; Conditions
   #:invalid-set-element #:invalid-tile-taken-from
   #:open-tile-not-in-set #:open-tile #:tiles
   #:invalid-shuntsu #:offending-tile
   #:set-reader-error #:offending-string
   #:invalid-kokushi-musou
   #:singles-set-contains-duplicates
   #:twelve-singles-and-pair-set-contains-duplicates #:pair-tile #:single-tiles
   #:invalid-puutaa #:offending-tiles
   #:full-hand-set-invalid-tile-count #:expected-tile-count
   ;; Protocol
   #:set #:tiles #:set= #:set-tile-count
   #:same-tile-set #:same-tile-set-tile
   #:closed-set #:open-set #:taken-from
   #:standard-set
   #:toitsu #:koutsu #:kantsu #:minkan #:shuntsu #:shuntsu-lowest-tile
   #:open-tile-set #:open-tile
   #:full-hand-set
   #:singles-set #:single-tiles
   #:twelve-singles-and-pair-set #:pair-tile
   #:fourteen-singles-set #:single-tiles
   #:kokushi-musou
   #:puutaa
   ;; Concrete classes
   #:antoi #:mintoi
   #:ankou #:minkou
   #:ankan #:daiminkan #:shouminkan
   #:anjun #:minjun
   #:closed-kokushi-musou #:open-kokushi-musou
   #:shiisan-puutaa #:shiisuu-puutaa
   ;; Set reader and printer
   #:print-set #:read-set #:read-set-from-string #:make-set
   ;; Tile-set matcher
   #:try-make-winning-set-from-tiles #:try-make-nonwinning-set-from-tiles
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
     (let ((tile (offending-tile condition)))
       (format stream "Attempted to make a shuntsu with ~A tile ~A."
               (if (suited-p tile) "lowest" "honor") tile)))))

(define-condition set-reader-error (riichi-evaluator-error)
  ((%offending-string :initarg :offending-string :accessor offending-string))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to read an invalid set: ~S"
             (offending-string condition)))))

(define-condition invalid-kokushi-musou (riichi-evaluator-error)
  ((%offending-tile :reader offending-tile :initarg :offending-tile))
  (:default-initargs
   :offending-tile (a:required-argument :offending-tile))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to make a kokushi musou with tile ~A."
             (offending-tile condition)))))

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
             (single-tiles condition)
             (pair-tile condition)))))

(define-condition invalid-puutaa (riichi-evaluator-error)
  ((%offending-tiles :reader offending-tiles :initarg :offending-tiles))
  (:default-initargs
   :offending-tiles (a:required-argument :offending-tiles))
  (:report
   (lambda (condition stream)
     (destructuring-bind (tile-1 tile-2) (offending-tiles condition)
       (format stream "Attempted to make a puutaa with neighboring tiles ~A ~
                      and ~A."
               tile-1
               tile-2)))))

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
                     which contains ~A than ~D tiles expected for that set."
             (tiles condition)
             (if (< (expected-tile-count condition) (length (tiles condition)))
                 "more" "fewer")
             (expected-tile-count condition)))))

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

(defmethod print-object ((set set) stream)
  (print-unreadable-object (set stream :type nil :identity nil)
    (format stream "~A " (type-of set))
    (print-set set stream)))

(p:define-protocol-class same-tile-set (set)
  ((%tile :reader same-tile-set-tile :initarg :tile))
  (:default-initargs
   :tile (a:required-argument :same-tile-set-tile)))

(defmethod initialize-instance :before ((set same-tile-set) &key tile)
  (unless (tile-p tile)
    (error 'invalid-set-element :datum tile :expected-type 'tile)))

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

(defmethod set= ((set-1 open-set) (set-2 open-set))
  (and (eq (taken-from set-1) (taken-from set-2))
       (call-next-method)))

(defmethod initialize-instance :after ((set open-set) &key taken-from)
  (unless (member taken-from *other-players*)
    (error 'invalid-tile-taken-from
           :datum taken-from
           :expected-type '#.`(member ,*other-players*))))

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

(defmethod initialize-instance :before ((set shuntsu) &key lowest-tile)
  (unless (tile-p lowest-tile)
    (error 'invalid-set-element :datum lowest-tile :expected-type 'tile))
  (unless (and (suited-p lowest-tile) (<= (rank lowest-tile) 7))
    (error 'invalid-shuntsu :offending-tile lowest-tile)))

(defmethod tiles ((set shuntsu))
  (let* ((tile-1 (shuntsu-lowest-tile set))
         (suit (suit tile-1)) (rank (rank tile-1))
         (tile-2 (make-instance 'suited-tile :suit suit :rank (+ 1 rank)))
         (tile-3 (make-instance 'suited-tile :suit suit :rank (+ 2 rank))))
    (list tile-1 tile-2 tile-3)))

(defmethod set= ((set-1 shuntsu) (set-2 shuntsu))
  (and (eq (class-of set-1) (class-of set-2))
       (tile= (shuntsu-lowest-tile set-1) (shuntsu-lowest-tile set-2))))

(p:define-protocol-class open-tile-set (open-set)
  ((%open-tile :reader open-tile :initarg :open-tile))
  (:default-initargs
   :open-tile (a:required-argument :open-tile)))

(defmethod initialize-instance :before ((set open-tile-set) &key open-tile)
  (unless (tile-p open-tile)
    (error 'invalid-set-element :datum open-tile :expected-type 'tile)))

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

(p:define-protocol-class puutaa (singles-set) ())

(defun tiles-pair-tile (tiles)
  (loop for tile in tiles
        when (< 1 (count tile tiles :test #'tile=))
          return tile))

(defun verify-puutaa-tiles (tiles)
  (a:when-let ((pair-tile (tiles-pair-tile tiles)))
    (return-from verify-puutaa-tiles (list pair-tile pair-tile)))
  (dolist (tile-1 tiles)
    (unless (honor-p tile-1)
      (dolist (tile-2 (remove tile-1 tiles :count 1 :test #'tile=))
        (unless (honor-p tile-2)
          (when (eq (suit tile-1) (suit tile-2))
            (let ((rank-difference (abs (- (rank tile-1) (rank tile-2)))))
              (when (< rank-difference 3)
                (return-from verify-puutaa-tiles (list tile-1 tile-2))))))))))

(p:define-protocol-class minkan (open-set kantsu) ())

;;; Concrete classes

(defclass antoi (closed-set toitsu) ())
(defun antoi (tile)
  (make-instance 'antoi :tile tile))

(defclass mintoi (open-set toitsu) ())
(defun mintoi (tile taken-from)
  (make-instance 'mintoi :tile tile :taken-from taken-from))

(defclass ankou (closed-set koutsu) ())
(defun ankou (tile)
  (make-instance 'ankou :tile tile))

(defclass minkou (open-set koutsu) ())
(defun minkou (tile taken-from)
  (make-instance 'minkou :tile tile :taken-from taken-from))

(defclass ankan (closed-set kantsu) ())
(defun ankan (tile)
  (make-instance 'ankan :tile tile))

(defclass daiminkan (minkan) ())
(defun daiminkan (tile taken-from)
  (make-instance 'daiminkan :tile tile :taken-from taken-from))

(defclass shouminkan (minkan) ())
(defun shouminkan (tile taken-from)
  (make-instance 'shouminkan :tile tile :taken-from taken-from))

(defclass anjun (closed-set shuntsu) ())
(defun anjun (lowest-tile)
  (make-instance 'anjun :lowest-tile lowest-tile))

(defclass minjun (open-tile-set shuntsu)
  ((%open-tile :reader open-tile :initarg :open-tile)))
(defun minjun (lowest-tile open-tile taken-from)
  (make-instance 'minjun :lowest-tile lowest-tile
                         :open-tile open-tile :taken-from taken-from))

(defmethod initialize-instance :after ((set minjun) &key)
  (let ((tile (open-tile set))
        (tiles (tiles set)))
    (unless (member tile tiles :test #'tile=)
      (error 'open-tile-not-in-set :open-tile tile :tiles tiles))))

(defmethod set= ((set-1 minjun) (set-2 minjun))
  (and (tile= (open-tile set-1) (open-tile set-2))
       (call-next-method)))

(defclass closed-kokushi-musou (closed-set kokushi-musou) ())
(defun closed-kokushi-musou (pair-tile)
  (make-instance 'closed-kokushi-musou :pair-tile pair-tile))

(defclass open-kokushi-musou (open-tile-set kokushi-musou) ())
(defun open-kokushi-musou (pair-tile open-tile taken-from)
  (make-instance 'open-kokushi-musou :pair-tile pair-tile :open-tile open-tile
                                     :taken-from taken-from))

(defmethod initialize-instance :after ((set open-kokushi-musou) &key)
  (let ((open-tile (open-tile set)))
    (unless (or (terminal-p open-tile) (honor-p open-tile))
      (error 'invalid-kokushi-musou :offending-tile open-tile))))

(defmethod set= ((set-1 open-kokushi-musou) (set-2 open-kokushi-musou))
  (and (tile= (open-tile set-1) (open-tile set-2))
       (call-next-method)))

(defclass shiisan-puutaa (closed-set puutaa twelve-singles-and-pair-set)
  ((%single-tiles :reader single-tiles :initarg :single-tiles))
  (:default-initargs
   :single-tiles (a:required-argument :single-tiles)))
(defun shiisan-puutaa (pair-tile single-tiles)
  (make-instance 'shiisan-puutaa :pair-tile pair-tile
                                :single-tiles single-tiles))

(defmethod initialize-instance :after ((set shiisan-puutaa) &key)
  (a:when-let ((offending-tiles (verify-puutaa-tiles (cons (pair-tile set)
                                                          (single-tiles set)))))
    (error 'invalid-puutaa :offending-tiles offending-tiles)))

(defmethod tiles ((set shiisan-puutaa))
  (sort (list* (pair-tile set) (pair-tile set) (copy-list (single-tiles set)))
        #'tile<))

(defmethod set= ((set-1 shiisan-puutaa) (set-2 shiisan-puutaa))
  (and (eq (class-of set-1) (class-of set-2))
       (tile= (pair-tile set-1) (pair-tile set-2))
       (tile-list= (single-tiles set-1) (single-tiles set-2))))

(defclass shiisuu-puutaa (closed-set puutaa fourteen-singles-set) ())
(defun shiisuu-puutaa (single-tiles)
  (make-instance 'shiisuu-puutaa :single-tiles single-tiles))

(defmethod initialize-instance :after ((set shiisuu-puutaa) &key)
  (a:when-let ((offending-tiles (verify-puutaa-tiles (single-tiles set))))
    (error 'invalid-puutaa :offending-tiles offending-tiles)))

(defmethod tiles ((set shiisuu-puutaa))
  (single-tiles set))

(defmethod set= ((set-1 shiisuu-puutaa) (set-2 shiisuu-puutaa))
  (and (eq (class-of set-1) (class-of set-2))
       (tile-list= (single-tiles set-1) (single-tiles set-2))))

(map nil #'mu:finalize-class-if-necessary (mu:subclasses 'set))

;;; Set printer

(defmethod print-set-using-class ((set closed-set) stream)
  (print-tile-list (tiles set) stream))

(defmethod print-set-using-class ((set mintoi) stream)
  (print-tile-list (tiles set) stream (case (taken-from set)
                                        (:kamicha '(0))
                                        (:toimen '(0 1))
                                        (:shimocha '(1)))))

(defmethod print-set-using-class ((set minkou) stream)
  (print-tile-list (tiles set) stream (case (taken-from set)
                                        (:kamicha '(0))
                                        (:toimen '(1))
                                        (:shimocha '(2)))))

(defmethod print-set-using-class ((set daiminkan) stream)
  (print-tile-list (tiles set) stream (case (taken-from set)
                                        (:kamicha '(0))
                                        (:toimen '(1))
                                        (:shimocha '(3)))))

(defmethod print-set-using-class ((set shouminkan) stream)
  (apply #'print-tile-list (tiles set) stream (case (taken-from set)
                                                (:kamicha '((0) (1)))
                                                (:toimen '((1) (2)))
                                                (:shimocha '((2) (3))))))

(defmethod print-set-using-class ((set minjun) stream)
  (let* ((open-tile (open-tile set))
         (tiles (cons open-tile (remove open-tile (tiles set) :test #'tile=))))
    (destructuring-bind (tile-1 tile-2 tile-3) tiles
      (let ((result-tiles (case (taken-from set)
                            (:kamicha (list tile-1 tile-2 tile-3))
                            (:toimen (list tile-2 tile-1 tile-3))
                            (:shimocha (list tile-2 tile-3 tile-1))))
            (result-flips (case (taken-from set)
                            (:kamicha '(0))
                            (:toimen '(1))
                            (:shimocha '(2)))))
        (print-tile-list result-tiles stream result-flips nil)))))

(defmethod print-set-using-class ((set open-kokushi-musou) stream)
  (let* ((open-tile (open-tile set))
         (tiles (remove open-tile (tiles set) :test #'tile= :count 1)))
    (destructuring-bind (first . rest) tiles
      (let ((result-tiles (case (taken-from set)
                            (:kamicha (list* open-tile first rest))
                            (:toimen (list* first open-tile rest))
                            (:shimocha (append tiles (list open-tile)))))
            (result-flips (case (taken-from set)
                            (:kamicha '(0))
                            (:toimen '(1))
                            (:shimocha '(13)))))
        (print-tile-list result-tiles stream result-flips nil)))))

;;; Set reader - string to parsed tiles

(defun parse-set-string (string)
  ;; NOTE: This returns a list of parsed tile represetations. Each parsed tile
  ;;       is a three-element list (RANK SUIT STATE), where RANK is an integer
  ;;       from 1 to 9, SUIT is one of (:NUMBER :CIRCLE :BAMBOO :HONOR), and
  ;;       STATE is one of (NIL :FLIP :SHOUMINKAN).
  (prog ((stack '())
         (result '())
         (i 0)
         (length (length string))
         char)
   :start
     (when (= i length) (go :end))
   :pop-char
     (setf char (char string i))
     (cond ((digit-char-p char) (go :number-char))
           ((alpha-char-p char) (go :alpha-char))
           ((char= #\* char) (go :flip-last-number))
           (t (go :error)))
   :number-char
     (let ((rank (digit-char-p char)))
       (push (list rank nil) stack))
     (go :loop)
   :alpha-char
     (dolist (elt (nreverse stack))
       (destructuring-bind (number state) elt
         (let ((suit (a:rassoc-value *print-table* char
                                     :test #'char-equal)))
           (unless suit (go :error))
           (push (list number suit state) result))))
     (setf stack '())
     (go :loop)
   :flip-last-number
     (unless stack (go :error))
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
     (return (nreverse result))
   :error
     (error 'riichi-evaluator-error)))

;;; Set reader - parsed tiles to set

(defun read-set (stream)
  (let ((string (loop for char = (peek-char t stream nil :eof t)
                      while (or (alphanumericp char) (eql char #\*))
                      collect (read-char stream t nil t))))
    (read-set-from-string string)))

(defun read-set-from-string (string)
  (flet ((complain () (error 'set-reader-error :offending-string string)))
    (handler-case (or (try-read-set (parse-set-string string)) (complain))
      (riichi-evaluator-error () (complain)))))

(defun make-set (string)
  (read-set-from-string string))

(defun try-read-make-tile (rank suit)
  (if (eq suit :honor)
      (make-instance 'honor-tile :kind (nth (1- rank) *honors*))
      (make-instance 'suited-tile :suit suit :rank rank)))

(defgeneric try-read-set (ordered)
  (:method-combination chained-append))

(defmethod try-read-set :toitsu (ordered)
  (when (= 2 (length ordered))
    (destructuring-bind ((rank-1 suit-1 state-1)
                         (rank-2 suit-2 state-2))
        ordered
      (when (and (= rank-1 rank-2)
                 (eq suit-1 suit-2))
        (let ((tile (try-read-make-tile rank-1 suit-1)))
          (a:switch ((list state-1 state-2) :test #'equal)
            ('(nil nil) (list (antoi tile)))
            ('(:flip nil) (list (mintoi tile :kamicha)))
            ('(:flip :flip) (list (mintoi tile :toimen)))
            ('(nil :flip) (list (mintoi tile :shimocha)))))))))

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
            ('(nil nil nil) (list (ankou tile)))
            ('(:flip nil nil) (list (minkou tile :kamicha)))
            ('(nil :flip nil) (list (minkou tile :toimen)))
            ('(nil nil :flip) (list (minkou tile :shimocha)))))))))

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
            ('(nil nil nil nil) (list (ankan tile)))
            ('(:flip nil nil nil) (list (daiminkan tile :kamicha)))
            ('(nil :flip nil nil) (list (daiminkan tile :toimen)))
            ('(nil nil nil :flip) (list (daiminkan tile :shimocha)))
            ('(:flip :shouminkan nil nil) (list (shouminkan tile :kamicha)))
            ('(nil :flip :shouminkan nil) (list (shouminkan tile :toimen)))
            ('(nil nil :flip :shouminkan)
              (list (shouminkan tile :shimocha)))))))))

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
                  (list (minjun lowest-tile open-tile taken-from))))
              (list (anjun lowest-tile))))))))

(defun good-kokushi-musou-p (tiles)
  (and (= 14 (length tiles))
       (let ((new-tiles (copy-list tiles)))
         (dolist (tile *kokushi-musou-tiles*)
           (a:deletef new-tiles tile :test #'tile= :count 1))
         (and (= 1 (length new-tiles))
              (member (first new-tiles) *kokushi-musou-tiles* :test #'tile=)))))

(defmethod try-read-set :kokushi-musou (ordered)
  (when (= 14 (length ordered))
    (let ((tiles (mapcar (lambda (x) (try-read-make-tile (first x) (second x)))
                         ordered)))
      (when (good-kokushi-musou-p tiles)
        (let ((pair-tile (tiles-pair-tile tiles)))
          (if (member :flip ordered :key #'third)
              (when (and (= 1 (count :flip ordered :key #'third))
                         (= 13 (count nil ordered :key #'third)))
                (let* ((open-position (position :flip ordered :key #'third))
                       (open-parsed-tile (nth open-position ordered))
                       (open-rank (first open-parsed-tile))
                       (open-suit (second open-parsed-tile))
                       (open-tile (try-read-make-tile open-rank open-suit))
                       (taken-from (ecase open-position
                                     (0 :kamicha)
                                     (1 :toimen)
                                     (13 :shimocha))))
                  (list (open-kokushi-musou pair-tile open-tile taken-from))))
              (list (closed-kokushi-musou pair-tile))))))))

(defmethod try-read-set :shiisan-puutaa (ordered)
  (when (and (= 14 (length ordered))
             (= 14 (count nil ordered :key #'third)))
    (let ((tiles (mapcar (lambda (x) (try-read-make-tile (first x) (second x)))
                         ordered)))
      (a:when-let ((pair-tile (tiles-pair-tile tiles)))
        (let ((single-tiles (remove pair-tile tiles :test #'tile= :count 1)))
          (when (null (verify-puutaa-tiles single-tiles))
            (list (shiisan-puutaa
                   pair-tile
                   (remove pair-tile single-tiles :test #'tile=
                                                  :count 1)))))))))

(defmethod try-read-set :shiisuu-puutaa (ordered)
  (when (and (= 14 (length ordered))
             (= 14 (count nil ordered :key #'third)))
    (let ((tiles (mapcar (lambda (x) (try-read-make-tile (first x) (second x)))
                         ordered)))
      (when (and (= 14 (length (remove-duplicates tiles :test #'tile=)))
                 (null (verify-puutaa-tiles tiles)))
        (list (shiisuu-puutaa tiles))))))

;;; Tile-set matcher - set makers

(defun try-make-shuntsu (tiles tile forbidden-sets class args)
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

(defun try-make-same-tile-set (tiles tile forbidden-sets class args)
  (let ((tile-count (set-tile-count (m:class-prototype (find-class class)))))
    (when (<= tile-count (count tile tiles :test #'tile=))
      (let ((set (apply class tile args)))
        (unless (member set forbidden-sets :test #'set=)
          (list set (remove tile tiles :count tile-count :test #'tile=)))))))

(defun try-make-kokushi-musou (tiles tile forbidden-sets class args)
  (declare (ignore tile))
  (when (good-kokushi-musou-p tiles)
    (let ((pair-tile (tiles-pair-tile tiles)))
      (let ((set (apply class pair-tile args)))
        (unless (member set forbidden-sets :test #'set=)
          (list set '()))))))

(defun try-make-shiisan-puutaa (tiles tile forbidden-sets class args)
  (declare (ignore tile args))
  (when (= 14 (length tiles))
    (a:when-let* ((pair-tile (tiles-pair-tile tiles))
                  (rest (remove pair-tile tiles :count 1 :test #'tile=)))
      (when (null (verify-puutaa-tiles rest))
        (let ((set (funcall class pair-tile
                            (remove pair-tile rest :count 1 :test #'tile=))))
          (unless (member set forbidden-sets :test #'set=)
            (list set '())))))))

(defun try-make-shiisuu-puutaa (tiles tile forbidden-sets class args)
  (declare (ignore tile args))
  (when (and (= 14 (length tiles))
             (null (verify-puutaa-tiles tiles)))
    (let ((set (funcall class tiles)))
      (unless (member set forbidden-sets :test #'set=)
        (list set '())))))

;;; Tile-set matcher - nonwinning sets

(defun try-make-nonwinning-set (tiles forbidden-sets class make-fn &rest args)
  (multiple-value-or
    (loop for tile in tiles
          for result = (funcall make-fn tiles tile forbidden-sets class args)
          when result return (values-list result))
    (values nil tiles)))

(defgeneric try-make-nonwinning-set-from-tiles (tiles forbidden-sets)
  (:method-combination chained-or))

(macrolet
    ((make-free-set (class make-fn)
       `(defmethod try-make-nonwinning-set-from-tiles ,class
          (tiles forbidden-sets)
          (try-make-nonwinning-set tiles forbidden-sets ',class ,make-fn))))
  (make-free-set antoi #'try-make-same-tile-set)
  (make-free-set ankou #'try-make-same-tile-set)
  (make-free-set anjun #'try-make-shuntsu))

;;; Tile-set matcher - winning sets

(defun try-make-winning-set
    (tiles winning-tile forbidden-sets class make-fn &rest args)
  (multiple-value-or
    (let ((new-tiles (cons winning-tile tiles)))
      (a:when-let ((result (funcall make-fn new-tiles winning-tile
                                    forbidden-sets class args)))
        (values-list (append result (list nil)))))
    (values nil tiles winning-tile)))

(defgeneric try-make-winning-set-from-tiles
    (tiles winning-tile win-from forbidden-sets)
  (:method-combination chained-or))

(macrolet
    ((make-tsumo-set (class make-fn)
       `(defmethod try-make-winning-set-from-tiles ,class
          (tiles winning-tile win-from forbidden-sets)
          (if (eq win-from :tsumo) ;; NOTE: this is a typecheck.
              (try-make-winning-set tiles winning-tile
                                    forbidden-sets ',class ,make-fn)
              (values nil tiles winning-tile)))))
  (make-tsumo-set antoi #'try-make-same-tile-set)
  (make-tsumo-set ankou #'try-make-same-tile-set)
  (make-tsumo-set anjun #'try-make-shuntsu)
  (make-tsumo-set closed-kokushi-musou #'try-make-kokushi-musou)
  (make-tsumo-set shiisan-puutaa #'try-make-shiisan-puutaa)
  (make-tsumo-set shiisuu-puutaa #'try-make-shiisuu-puutaa))

(macrolet
    ((make-ron-set (class make-fn &rest args)
       `(defmethod try-make-winning-set-from-tiles ,class
          (tiles winning-tile win-from forbidden-sets)
          (if (member win-from *other-players*) ;; NOTE: this is a typecheck.
              (try-make-winning-set tiles winning-tile
                                    forbidden-sets ',class ,make-fn ,@args)
              (values nil tiles winning-tile)))))
  (make-ron-set mintoi #'try-make-same-tile-set win-from)
  (make-ron-set minkou #'try-make-same-tile-set win-from)
  (make-ron-set minjun #'try-make-shuntsu winning-tile win-from)
  (make-ron-set open-kokushi-musou #'try-make-kokushi-musou
                winning-tile win-from))

