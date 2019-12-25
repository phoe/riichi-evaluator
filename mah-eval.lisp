;;; src/mah-eval.lisp
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

(defpackage #:riichi-evaluator
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:riichi-evaluator)

;;; Tile tables

(defparameter *suit-table*
  '((#\M . :number)
    (#\P . :circle)
    (#\S . :bamboo)))

(defparameter *wind-table*
  '((#\E . :east)
    (#\S . :south)
    (#\W . :west)
    (#\N . :north)))

(defparameter *dragon-table*
  '((#\B . :haku)
    (#\F . :hatsu)
    (#\C . :chun)))

(defparameter *dragon-print-table*
  '(("Hk" . :haku)
    ("Ht" . :hatsu)
    ("Ch" . :chun)))

(defparameter *honor-table*
  (append *wind-table* *dragon-table*))

;;; Tile protocol

(defclass tile () ())

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

(defgeneric tile-equal (tile1 tile2)
  (:method ((tile1 tile) (tile2 tile)) nil))

(defgeneric tile-consec-p (tile1 tile2 &key wrap-around)
  (:method ((tile1 tile) (tile2 tile) &key wrap-around)
    (declare (ignore wrap-around))
    nil))

(defgeneric tile< (tile1 tile2))

(defgeneric format-tile (tile))

;;; Suited tile

(defclass suited-tile (tile)
  ((suit :initarg :suit
         :reader suit)
   (rank :initarg :rank
         :reader rank)))

(defmethod print-object ((tile suited-tile) stream)
  (format stream "[~D~C]"
          (rank tile)
          (char-downcase (a:rassoc-value *suit-table* (suit tile)))))

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

(defmethod tile-equal ((a suited-tile) (b suited-tile))
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
  ((kind :initarg :kind
         :reader kind)))

(defmethod honor-p ((tile honor-tile))
  t)

(defmethod wind-p ((tile honor-tile))
  (position (kind tile) *wind-table* :key #'cdr))

(defmethod dragon-p ((tile honor-tile))
  (position (kind tile) *dragon-table* :key #'cdr))

(defmethod print-object ((tile honor-tile) stream)
  (let ((text (cond ((wind-p tile)
                     (a:rassoc-value *honor-table* (kind tile)))
                    ((dragon-p tile)
                     (a:rassoc-value *dragon-print-table* (kind tile))))))
    (format stream "[~A]" text)))

(defmethod of-wind (wind (tile honor-tile))
  (equal wind (kind tile)))

(defmethod tile-equal ((a honor-tile) (b honor-tile))
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
                          ((dragon-p t1) (mapcar #'cdr *dragon-table*))
                          (t (error 'invalid-tile :symbol (kind t1)))))
             (succ (next-with-rollover (kind t1) table)))
        (equal succ (kind t2))))))

;;; Sorting order for suited and honor tiles.

(defmethod tile< ((a suited-tile) (b honor-tile)) t)

(defmethod tile< ((a honor-tile) (b suited-tile)) nil)

;; Tile parser

(define-condition invalid-tile (error)
  ((symbol :initarg :symbol
           :reader invalid-tile-symbol)))

(defun parse-tile (symbol)
  (flet ((complain () (error 'invalid-tile :symbol symbol)))
    (handler-case
        (let ((text (string symbol)))
          (case (length text)
            (2 (make-instance 'suited-tile
                              :suit (a:assoc-value *suit-table* (char text 0))
                              :rank (digit-char-p (char text 1))))
            (1 (make-instance 'honor-tile
                              :kind (a:assoc-value *honor-table* (char text 0))))
            (t (complain))))
      (type-error () (complain)))))

;;; Set parser

(define-condition invalid-set (error)
  ((set :initarg :list
        :reader invalid-set-list)
   (reason :initarg :reason
           :reader invalid-set-reason)))

(defun parse-set (seq)
  (flet ((tile-equal-all (tiles)
           (every (a:curry #'tile-equal (first tiles)) (rest tiles)))
         (tile-consec-p-all (tiles)
           (loop for (first second) on tiles
                 while first while second
                 always (tile-consec-p first second))))
    (let* ((match (case (first seq)
                    (open :open)
                    (closed :closed)
                    (t nil)))
           (open-or-closed (or match :open))
           (tiles (sort (mapcar #'parse-tile (if match (rest seq) seq)) #'tile<))
           (type (case (length tiles)
                   (2 'pair)
                   (3 (cond ((tile-equal-all tiles) 'pon)
                            ((tile-consec-p-all tiles) 'chi)
                            (t 'unknown)))
                   (4 'kan)
                   (t 'unknown))))
      (case type
        ((pair pon kan)
         (unless (tile-equal-all tiles)
           (error 'invalid-set :list seq :reason "The tiles are not the same.")))
        (chi
         (unless (tile-consec-p-all tiles)
           (error 'invalid-set :list seq :reason "Tiles are not consecutive.")))
        (t
         (error 'invalid-set :list seq :reason "Not pair, chi, pon or kan.")))
      (when (and (equal :open open-or-closed) (equal 'pair type))
        (error 'invalid-set :list seq :reason "A pair cannot be open."))
      (list* open-or-closed type tiles))))

;;; Hand parser

(define-condition invalid-hand (error)
  ((reason :initarg :reason
           :initform "No reason given."
           :reader reason)))

(define-condition invalid-hand-element (invalid-hand)
  ((element :initarg :element
            :reader invalid-hand-element-element)
   (value :initarg :value
          :reader invalid-hand-element-value)
   (problem :initarg :problem
            :initform nil
            :reader invalid-hand-element-problem)))

(defmethod reason ((c invalid-hand-element))
  (format nil "Invalid ~a ~a~:[~;: ~:*~a~]."
          (invalid-hand-element-element c)
          (with-output-to-string (out) (princ (invalid-hand-element-value c) out))
          (invalid-hand-element-problem c)))

(defun parse-tiles (seq)
  (loop for item in seq
        if (listp item)
          collect (parse-set item) into locked-sets
        if (symbolp item)
          collect (parse-tile item) into free-tiles
        finally (return (values free-tiles locked-sets))))

(defun parse-dora-list (which seq)
  (handler-case (mapcar #'parse-tile seq)
    (invalid-tile (c) (error 'invalid-hand-element
                             :element which
                             :value (invalid-tile-symbol c)))
    (type-error (c) (error 'invalid-hand-element
                           :element (format nil "~a list" which)
                           :value (type-error-datum c)))))

;;; Set

(defun set-open-closed (set)
  (first set))

(defun (setf set-open-closed) (newval set)
  (setf (first set) newval))

(define-condition unknown-kind-of-set (error)
  ((kind :initarg kind)))

(defun set-open-closed-p (open-closed set)
  (or (equal open-closed :all)
      (equal open-closed (first set))))

(defun set-closed-p (set)
  (equal :closed (first set)))

(defun set-type (set)
  (second set))

(defun set-kan-p (set)
  (equal 'kan (set-type set)))

(defun format-set (set)
  (format nil "(~(~a ~a~)~{ ~a~})" (set-open-closed set) (set-type set) (mapcar #'format-tile (set-tiles set))))

;;; Hand

(defclass hand ()
  ((prevailing-wind :accessor hand-prevailing-wind :initarg :prevailing-wind)
   (seat-wind :accessor hand-seat-wind :initarg :seat-wind)
   (self-draw :accessor hand-self-draw :initarg :self-draw)
   (winning-tile :accessor hand-winning-tile :initarg :winning-tile)
   (tiles :accessor hand-tiles :initarg :tiles)
   (locked-sets :accessor hand-locked-sets :initarg :locked-sets)
   (free-tiles :accessor hand-free-tiles :initarg :free-tiles)
   (closed :accessor hand-closed :initarg :closed)
   (riichi :accessor hand-riichi :initarg :riichi)
   (doras :accessor hand-doras :initarg :doras)
   (ura-doras :accessor hand-ura-doras :initarg :ura-doras)))

(defgeneric copy-hand (hand)
  (:method ((hand hand))
    (with-slots (prevailing-wind seat-wind self-draw winning-tile tiles
                 locked-sets free-tiles closed riichi doras ura-doras)
        hand
      (make-instance
       'hand
       :prevailing-wind prevailing-wind :seat-wind seat-wind
       :self-draw self-draw :winning-tile winning-tile
       :tiles tiles :locked-sets locked-sets :free-tiles free-tiles
       :closed closed :riichi riichi :doras doras :ura-doras ura-doras))))

(defun parse-hand (seq)
  (labels ((error-ood (which)
             (error 'invalid-hand
                    :reason (format nil "Ran out of data before ~a" which)))
           (peek (which)
             (if seq (first seq) (error-ood which)))
           (next (which)
             (if seq (pop seq) (error-ood which)))
           (read-wind (which) (let ((wind (next which)))
                                (case wind
                                  ((east e) :east)
                                  ((south s) :south)
                                  ((west w) :west)
                                  ((north n) :north)
                                  (t (error 'invalid-hand-element
                                            :element which
                                            :value wind))))))
    (let* ((prevailing-wind (read-wind "prevailing wind"))
           (seat-wind (read-wind "seat wind"))
           (doras (parse-dora-list "dora" (next "dora list")))
           (riichi (listp (peek "tsumo/ron indicator or ura dora list")))
           (ura-doras (when riichi (parse-dora-list "ura dora" (next "ura dora list"))))
           (self-draw (let ((tsumo-ron (next "tsumo/ron indicator")))
                        (case tsumo-ron
                          ((tsumo t) t)
                          ((ron r) nil)
                          (t (error 'invalid-hand-element
                                    :element "tsumo/ron indicator"
                                    :value tsumo-ron
                                    :problem "Not \"tsumo\", \"t\", \"ron\" or \"r\""))))))
      (multiple-value-bind (free-tiles locked-sets)
          (handler-case (parse-tiles seq)
            (invalid-set (c) (error 'invalid-hand-element :element "set"
                                                          :value (invalid-set-list c)
                                                          :problem (invalid-set-reason c)))
            (invalid-tile (c) (error 'invalid-hand-element :element "tile"
                                                           :value (invalid-tile-symbol c))))
        (cond ((not (or free-tiles locked-sets))
               (error 'invalid-hand :reason "No tiles specified."))
              ((not free-tiles)
               (error 'invalid-hand
                      :reason "The winning tile must be be specified and not in a set.")))
        (let* ((tiles-in-locked-sets (mapcan #'set-tiles locked-sets))
               (winning-tile (car (last free-tiles)))
               (tiles (append free-tiles tiles-in-locked-sets))
               (closed (every #'set-closed-p locked-sets)))
          (when (and riichi (not closed))
            (error 'invalid-hand :reason "Riichi with open hand"))
          (let ((tile-count (+ (length free-tiles)
                               (loop for set in locked-sets
                                     sum (ecase (set-type set)
                                           ((chi pon kan) 3)
                                           ('pair 2))))))
            (when (/= tile-count 14)
              (error 'invalid-hand
                     :reason (format nil "Too ~A tiles (~d)"
                                     (if (< tile-count 14) "few" "many")
                                     tile-count))))
          (make-instance 'hand :prevailing-wind prevailing-wind
                               :seat-wind seat-wind
                               :self-draw self-draw
                               :winning-tile winning-tile
                               :tiles (sort tiles #'tile<)
                               :locked-sets locked-sets
                               :free-tiles (sort free-tiles #'tile<)
                               :closed closed
                               :riichi riichi
                               :doras doras
                               :ura-doras ura-doras))))))



























;;; Pattern matchers

(defparameter *pattern-matchers* (list))

(defun match-and-consume (matcher tiles)
  (multiple-value-bind (pat new-tiles) (funcall matcher tiles)
    (if pat
        (values pat (if (listp new-tiles) new-tiles (subseq tiles new-tiles)))
        (values nil nil))))

(defun add-to-paths (pattern paths)
  (if paths
      (loop for path in paths
            collect (cons pattern path))
      (list (list pattern))))

(defun match-recursive (matcher tiles)
  (multiple-value-bind (pattern remainder) (match-and-consume matcher tiles)
    (when pattern
      (let ((remainder-paths (sub-paths remainder)))
        (add-to-paths pattern remainder-paths)))))

(defun sub-paths (tiles)
  (when tiles
    (loop for m in *pattern-matchers*
          if (match-recursive m tiles)
            nconc it)))

(defmacro with-hand-helpers (hand-name ord-name &rest forms)
  `(let* ((prevailing-wind (hand-prevailing-wind ,hand-name))
          (seat-wind (hand-seat-wind ,hand-name))
          (self-draw (hand-self-draw ,hand-name))
          (winning-tile (hand-winning-tile ,hand-name))
          (winning-set (find-if (lambda (set) (find winning-tile (set-tiles set))) ,ord-name))
          (tiles (hand-tiles ,hand-name))
          (closed (hand-closed ,hand-name)))
     (declare (ignorable prevailing-wind seat-wind self-draw winning-tile
                         winning-set tiles closed))
     (labels ((lambda-p (open-closed types)
                (lambda (set) (and (set-open-closed-p open-closed set)
                                   (find (set-type set) types))))
              (sets (open-closed &rest types)
                (remove-if-not (lambda-p open-closed types) ,ord-name))
              (count-sets (open-closed &rest types)
                (count-if (lambda-p open-closed types) ,ord-name))
              (of-good-wind-p (tile)
                (or (of-wind prevailing-wind tile) (of-wind seat-wind tile))))
       (declare (ignorable #'sets #'count-sets #'of-good-wind-p))
       ,@forms)))

(defun count-fu-melds (sets)
  (loop for set in sets
        for tile = (set-first set)
        sum (* (if (simple-p tile) 2 4)
               (if (set-closed-p set) 2 1)
               (if (set-kan-p set) 4 1))))

(defun count-fu-pairs (pairs of-good-wind-p)
  (* 2 (count-if (lambda (pair) (let ((tile (set-first pair)))
                                  (or (dragon-p tile)
                                      (funcall of-good-wind-p tile)))) pairs)))

(defun count-fu-waits (winning-set winning-tile)
  (case (set-type winning-set)
    (pair 2)                            ; pair wait
    (chi (let* ((tiles (set-tiles winning-set))
                (pos (position winning-tile tiles))
                (rank (set-rank winning-set)))
           (ecase pos
             (2 (if (= rank 1) 2 0))    ; edge wait     1   2  <3>
             (1 2)                      ; closed wait
             (0 (if (= rank 7) 2 0))))) ; edge wait    <7>  8   9
    (t 0)))

(defun seven-unique-pairs (pairs)
  (and (= 7 (length pairs))
       (no-identical-sets pairs)))

(defun count-fu (hand ord)
  (with-hand-helpers hand ord
    (let ((pairs (sets :all 'pair)))
      (if (seven-unique-pairs pairs)
          (values 25 -1)
          (let* ((for-melds (count-fu-melds (sets :all 'pon 'kan)))
                 (for-pairs (count-fu-pairs pairs #'of-good-wind-p))
                 (for-waits (count-fu-waits winning-set winning-tile))
                 (for-all-of-above (+ for-melds for-pairs for-waits))
                 (pinfu-p (zerop for-all-of-above))
                 (for-self-draw (if (and self-draw (not pinfu-p)) 2 0))
                 (for-open-pinfu (if (and pinfu-p (not closed)) 2 0))
                 (base-total (+ for-all-of-above for-self-draw for-open-pinfu))
                 (for-winning (if (and closed (not self-draw)) 30 20))
                 (total (+ base-total for-winning)))
            (values (* 10 (ceiling total 10))
                    base-total))))))

(defun qualify-ord (ord)
  (mapcar (lambda (set) (cons :closed set)) ord))

(defun full-ord (path locked-sets)
  (append locked-sets path))

(defun combine-han (a b)
  (flet ((either (a b target)
           (or (equal a target) (equal b target))))
    (cond
      ((either a b :double-yakuman) :double-yakuman)
      ((either a b :yakuman) :yakuman)
      ((> (+ a b) 13) 13) ; limit han at 13
      (t (+ a b)))))

(defun combine-yakulists (&optional a b)
  (flet ((yakulist-han (yakulist) (first yakulist))
         (yakulist-yakus (yakulist) (rest yakulist)))
    (cond ((not a) (list 0))
          (t  (let ((new-han (combine-han (yakulist-han a) (yakulist-han b)))
                    (new-yakus (nconc (yakulist-yakus a) (yakulist-yakus b))))
                (cons new-han new-yakus))))))

(defstruct scoring
  ord
  han
  fu
  yakus)

(defun scoring-add-han (scoring han for &key prepend)
  (if (and (plusp han) for)
      (let* ((combined-han (combine-han (scoring-han scoring) han))
             (new-yakus (if (listp for) for (list for)))
             (old-yakus (scoring-yakus scoring))
             (combined-yakus (if prepend
                                 (nconc new-yakus old-yakus)
                                 (nconc old-yakus new-yakus))))
        (make-scoring :ord (scoring-ord scoring) :han combined-han
                      :fu (scoring-fu scoring) :yakus combined-yakus))
      scoring))

(defun higher-scoring (&optional a b)
  (if (and a b)
      (let ((han-a (scoring-han a))
            (han-b (scoring-han b)))
        (cond
          ((equal han-a :double-yakuman) a)
          ((equal han-b :double-yakuman) b)
          ((equal han-a :yakuman) a)
          ((equal han-b :yakuman) b)
          ((> han-a han-b) a)
          ((< han-a han-b) b)
          ((> (scoring-fu b) (scoring-fu a)) b)
          (t a)))
      (list 0 0)))

;;; Yaku definition

(defparameter *yaku-matchers* (list))
(defparameter *yaku-list* (list))

(defun limit (han fu)
  (case (if (and (numberp han) (> han 13)) 13 han)
    (:double-yakuman :double-yakuman)
    (:yakuman :yakuman)
    (13 :kazoe-yakuman)
    ((11 12) :sanbaiman)
    ((8 9 10) :baiman)
    ((6 7) :haneman)
    (5 :mangan)
    (4 (when (>= fu 40) :kazoe-mangan))
    (3 (when (>= fu 70) :kazoe-mangan))))

(defun list-yakus ()
  (loop for (name han-closed han-open) in (reverse *yaku-list*)
        do (format t "~(~a~): ~(~a~) closed~[~:;, ~(~a~) open~]~%"
                   name han-closed (if (equal 0 han-open) 0 1) han-open)))

(defun count-doras-hits (doras tiles)
  (flet ((count-dora-hits (dora tiles)
           (count-if (lambda (tile) (tile-consec-p dora tile :wrap-around t)) tiles)))
    (loop for dora in doras sum (count-dora-hits dora tiles))))

(defun eval-ordering (hand ord)
  (multiple-value-bind (fu base-fu) (count-fu hand ord)
    (let* ((all-yakus (loop for yaku-p in *yaku-matchers*
                            if (funcall yaku-p hand ord base-fu)
                              nconc it))
           (combined (reduce #'combine-yakulists all-yakus)))
      (make-scoring :ord ord :han (first combined) :fu fu :yakus (rest combined)))))




(defun ordering-ready-p (hand ord)
  (with-hand-helpers hand ord
    (let ((chi-pon-kan (count-sets :all 'chi 'pon 'kan))
          (pair (count-sets :all 'pair)))
      (or
       (and (= 4 chi-pon-kan)
            (= 1 pair))
       (seven-unique-pairs (sets :all 'pair))
       (and (= 1 (count-sets :all 'twelve-singles))
            (= 1 pair)
            (not (some #'simple-p tiles)))))))


(defun consider-winning-tile (hand unlocked-sets ord)
  (let* ((winning-tile (hand-winning-tile hand))
         (self-draw (hand-self-draw hand))
         (possible-winning
           (loop for set in unlocked-sets
                 for tile = (find winning-tile (set-tiles set) :test #'tile-equal)
                 if tile collect (list set tile))))
    (loop for (win-set win-tile) in possible-winning
          for alt-hand = (copy-hand hand)
          for alt-ord = (loop for set in ord
                              for alt-set = (copy-list set)
                              if (and (not self-draw)
                                      (eq win-set set))
                                do (setf (set-open-closed alt-set) :open)
                              collect alt-set)
          do (setf (hand-winning-tile alt-hand) win-tile)
          collect (list alt-hand alt-ord win-set))))


(define-condition no-mahjong (invalid-hand)
  ((reason :initform "No mahjong.")))

(defun eval-all-orderings (hand)
  (let ((locked-sets (hand-locked-sets hand)))
    (loop for partial-ord in (sub-paths (hand-free-tiles hand))
          for qualified-partial-ord = (qualify-ord partial-ord)
          for full-ord = (full-ord qualified-partial-ord locked-sets)
          if (ordering-ready-p hand full-ord)
            nconc
          ;; If there are multiple free tiles equal to the winning tile,
          ;; all of them should be considered as the winning tile in turn.
            (loop for (alt-hand alt-ord) in (consider-winning-tile
                                             hand qualified-partial-ord full-ord)
                  collect (eval-ordering alt-hand alt-ord))
            into scorings
          finally (return (or scorings (error 'no-mahjong))))))

(defun add-yaku-to-list (name han-closed han-open)
  (push (list name han-closed han-open) *yaku-list*))

(defmacro define-yaku (name han-closed han-open &rest forms)
  `(progn
     (push
      (lambda (hand ord base-fu)
        (declare (ignorable base-fu))
        (with-hand-helpers hand ord
          (when (progn ,@forms)
            (let ((han (if closed ,han-closed ,han-open)))
              (unless (equal han 0)
                (list (list han (list ',name han))))))))
      *yaku-matchers*)
     (add-yaku-to-list ',name ,han-closed ,han-open)
     ',name))

(defmacro define-multi-yaku (&rest forms)
  `(push
    (lambda (hand ord base-fu)
      (declare (ignorable base-fu))
      (remove nil (with-hand-helpers hand ord
                    ,@forms)))
    *yaku-matchers*))


(defun yakuman-p (yaku)
  (find :yakuman yaku))

(defun double-yakuman-p (yaku)
  (find :double-yakuman yaku))

;; A pattern-matcher returns two values:
;; pat: a matched pattern or nil if no match
;; new-tiles: a list of remaining tiles or amount of tiles to be consumed
(defmacro define-pattern (name min-length &rest forms)
  (declare (ignore name))
  `(push (lambda (tiles)
           (when (>= (length tiles) ,min-length)
             ,@forms))
         *pattern-matchers*))

(defun count-consec-sames (tiles)
  (let ((len (length tiles)))
    (case len
      (0 (error 'no-tiles))
      (1 1)
      (t (or (position (first tiles) tiles :test-not #'tile-equal) len)))))

(defmacro define-sames-pattern (name length)
  `(define-pattern ,name ,length
     (if (<= ,length (count-consec-sames tiles))
         (values (cons ',name (subseq tiles 0 ,length)) ,length)
         (values nil 0))))

(define-sames-pattern pair 2)
(define-sames-pattern pon 3)

(defun remove-nth (n seq)
  (remove-if (constantly t) seq :start n :count 1))

(defun find-matching (predicate tiles max-length)
  (if (and tiles
           (> max-length 0))
      (let* ((t1 (first tiles))
             (t2-pos (position-if (lambda (t2) (funcall predicate t1 t2)) tiles :start 1))
             (skipped (if t2-pos (subseq tiles 1 t2-pos) (rest tiles)))
             (next-tiles (if t2-pos (subseq tiles t2-pos) (list))))
        (multiple-value-bind (next-found next-skipped)
            (find-matching predicate next-tiles (1- max-length))
          (values (cons t1 next-found) (nconc skipped next-skipped))))
      (values (list) tiles)))

(defun find-straight (tiles max-length)
  (find-matching (lambda (t1 t2) (and (suited-p t2) (tile-consec-p t1 t2))) tiles max-length))

(defmacro define-straight-pattern (name len)
  `(define-pattern ,name ,len
     (multiple-value-bind (found skipped) (find-straight tiles ,len)
       (if (= ,len (length found))
           (values (cons ',name found) skipped)
           (values nil 0)))))

(define-straight-pattern chi 3)

(define-pattern twelve-singles 12
  (multiple-value-bind (found skipped)
      (find-matching (lambda (t1 t2) (declare (ignore t1))
                       (= 1 (count t2 tiles :test #'tile-equal)))
                     tiles
                     12)
    (if (= 12 (length found))
        (values (cons 'twelve-singles found) skipped)
        (values nil 0))))

(defun chi-equal (c1 c2)
  (tile-equal (set-first c1) (set-first c2)))

(defun count-chi-pairs (chis)
  (if chis
      (let* ((c1 (first chis))
             (rest-chis (rest chis))
             (c2-pos (position-if (lambda (c2) (chi-equal c1 c2)) rest-chis))
             (next-chis (if c2-pos
                            (remove-if (lambda (x) t) rest-chis :start c2-pos :count 1)
                            rest-chis)))
        (+ (if c2-pos 1 0) (count-chi-pairs next-chis)))
      0))

(defun set-rank (chi)
  (rank (set-first chi)))

(defun set-suit (chi)
  (suit (set-first chi)))

(defun lambda-set-of-suit (suit)
  (lambda (set) (equal suit (set-suit set))))

(defun set-equal (s1 s2)
  (and (equal (set-type s1) (set-type s2))
       (let ((tiles-1 (set-tiles s1))
             (tiles-2 (set-tiles s2)))
         (loop for i from 0 upto (1- (length tiles-1))
               for t1 = (elt tiles-1 i)
               for t2 = (elt tiles-2 i)
               always (tile-equal t1 t2)))))

(defun set-equal-rank (c1 c2)
  (= (set-rank c1) (set-rank c2)))

(defun sanshoku (sets)
  (let ((len (length sets)))
    (when (>= 3 len)
      (loop for i from 0 upto (- len 3)
            for i-set = (elt sets i)
            for i-rank = (set-rank i-set)
            for i-suit = (set-suit i-set)
            for suits = (loop for j from (1+ i) below len
                              for j-set = (elt sets j)
                              for j-rank = (set-rank j-set)
                              for j-suit = (set-suit j-set)
                              if (and (= i-rank j-rank) (not (equal i-suit j-suit)))
                                collect j-suit)
            for diff-suits = (delete-duplicates suits)
              thereis (= 2 (length diff-suits))))))

(defun set-tiles (set)
  (copy-list (cddr set)))

(defun set-first (set)
  (third set))

(defun set-kind (set)
  (kind (set-first set)))

(defun all-of (predicate key amount sets)
  (loop for set in sets
        if (funcall predicate (set-first set)) collect set into type-sets
          finally (return (= amount
                             (length (delete-duplicates type-sets :key key))))))

(defun all-kinds-of-type (predicate amount sets)
  (all-of predicate #'set-kind amount sets))

(defgeneric green-p (tile)
  (:method ((tile tile)) nil)
  (:method ((tile suited-tile))
    (and (equal :bamboo (suit tile))
         (find (rank tile) '(2 3 4 6 8))))
  (:method ((tile honor-tile))
    (equal :hatsu (kind tile))))

(defun all-of-same-suit (tiles)
  (let ((first-suited (find-if #'suited-p tiles)))
    (when first-suited
      (let* ((suit (suit first-suited))
             (first-other-suited (find-if-not (a:curry #'of-suit suit) tiles)))
        (not first-other-suited)))))

(defun sets-of-rank (rank sets)
  (remove-if-not (lambda (set) (= rank (set-rank set))) sets))


(define-yaku riichi 1 0
  (hand-riichi hand))

(define-yaku menzen-tsumo 1 0
  (and self-draw closed))

(define-yaku tanyao 1 0
  (every #'simple-p tiles))

(define-yaku pinfu 1 0
  (zerop base-fu))

(define-yaku iipeikou 1 0
  (= 1 (count-chi-pairs (sets :all 'chi))))

(define-yaku sanshoku-doujun 2 1
  (sanshoku (sets :all 'chi)))

(define-yaku ittsuu 2 1
  (loop with chis = (sets :all 'chi)
        with bottoms = (sets-of-rank 1 chis)
        with middles = (sets-of-rank 4 chis)
        with tops = (sets-of-rank 7 chis)
        for bottom in bottoms
        for suit = (set-suit bottom)
          thereis (and (find-if (lambda-set-of-suit suit) middles)
                       (find-if (lambda-set-of-suit suit) tops))))

(loop for type in (list* :prevailing-wind :seat-wind (mapcar #'cdr *dragon-table*))
      for yaku-name = (format nil "FANPAI-~a" type)
      do (add-yaku-to-list (intern yaku-name) 1 1))

(define-multi-yaku
    (loop for set in (sets :all 'pon 'kan)
          for tile = (set-first set)
          for kind = (when (honor-p tile) (kind tile))
          for wind = (when (wind-p tile) kind)
          for type = (cond
                       ((dragon-p tile) kind)
                       ((and wind (equal wind prevailing-wind)) 'prevailing-wind)
                       ((and wind (equal wind seat-wind)) 'seat-wind))
          if (dragon-p tile) collect kind into dragon-types
            if (equal wind prevailing-wind) collect 'prevailing-wind into wind-types
              if (equal wind seat-wind) collect 'seat-wind into wind-types
                finally (return
                          (let ((all-types (if (= 3 (length dragon-types))
                                               wind-types
                                               (nconc dragon-types wind-types))))
                            (loop for type in all-types
                                  collect (list 1 (list (intern (format nil "FANPAI-~a" type))
                                                        1)))))))

(defun terminals-and-honors (tiles)
  (and (not (some #'simple-p tiles))
       (some #'terminal-p tiles)
       (some #'honor-p tiles)))

(defun no-identical-sets (sets)
  (= (length sets)
     (length (remove-duplicates sets :test #'set-equal))))


(define-yaku chanta 2 1
  (and (some #'terminal-p tiles)
       (some #'honor-p tiles)
       (plusp (count-sets :all 'chi))
       (every (lambda (set) (not (every #'simple-p (set-tiles set))))
              (sets :all 'pair 'chi 'pon 'kan))))

(define-yaku chiitoitsu 2 0
  (seven-unique-pairs (sets :all 'pair)))

(define-yaku sanshoku-doukou 2 2
  (sanshoku (remove-if-not
             (lambda (set) (suited-p (set-first set)))
             (sets :all 'pon))))

(define-yaku san-ankou 2 2
  (= 3 (count-sets :closed 'pon 'kan)))

(define-yaku san-kantsu 2 2
  (= 3 (count-sets :all 'kan)))

(define-yaku toitoi 2 2
  (= 4 (count-sets :all 'pon 'kan)))
                                        ;   (/= 4 (count-sets :closed 'pon 'kan)))) // cumulative?

(define-yaku honitsu 3 2
  (if (some #'honor-p tiles)
      (all-of-same-suit (remove-if #'honor-p tiles))))

(define-yaku shousangen 2 2
  (let ((pairs (sets :all 'pair)))
    (and (= 1 (length pairs))
         (dragon-p (set-first (first pairs)))
         (all-kinds-of-type #'dragon-p 3 (sets :all 'pair 'pon 'kan)))))

(define-yaku honroutou 2 2
  (terminals-and-honors tiles))

(define-yaku junchan 3 2
  (and (plusp (count-sets :all 'chi))
       (every (lambda (set) (some #'terminal-p (set-tiles set)))
              (sets :all 'pair 'chi 'pon 'kan))))

(define-yaku ryanpeikou 3 0
  (= 2 (count-chi-pairs (sets :all 'chi))))

(define-yaku chinitsu 6 5
  (all-of-same-suit tiles))

(define-yaku kokushi-musou :yakuman 0
  (and (not (some #'simple-p tiles))
       (= 1 (count-sets :all 'twelve-singles))
       (= 1 (count-sets :all 'pair))))

(define-yaku suu-ankou :yakuman :yakuman
  (= 4 (count-sets :closed 'pon 'kan)))

(define-yaku daisangen :yakuman :yakuman
  (all-kinds-of-type #'dragon-p 3 (sets :all 'pon 'kan)))

(define-yaku shousuushii :yakuman :yakuman
  (and (all-kinds-of-type #'wind-p 4 (sets :all 'pair 'pon 'kan))
       (= 1 (count-sets :all 'pair))))

(define-yaku daisuushii :double-yakuman :double-yakuman
  (all-kinds-of-type #'wind-p 4 (sets :all 'pon 'kan)))

(define-yaku tsuuiisou :yakuman :yakuman
  (every #'honor-p tiles))

(define-yaku chinroutou :yakuman :yakuman
  (every #'terminal-p tiles))

(define-yaku ryuuiisou :yakuman :yakuman
  (every #'green-p tiles))

(define-yaku chuuren-poutou :yakuman 0
  (and (all-of-same-suit tiles)
       (let ((ranks (mapcar #'rank tiles)))
         (and (= 3 (count 1 ranks))
              (= 3 (count 9 ranks))))))

(define-yaku suu-kantsu :yakuman :yakuman
  (= 4 (count-sets :all 'kan)))

(defparameter *old-test-hands*
  '((east east ron M1 M2 M3 P2 P3 P4 S3 S4 S5 (open M6 M7 M8) P7 P7)  ; nothing
    (east east ron M3 M3 P1 P1 P5 P5 S1 S1 S8 E E C C S8)       ; chiitoitsu
    (east east tsumo M1 M2 M3 M5 M5 P3 P4 P7 P8 P9 S4 S5 S6 P2)     ; pinfu
    (east east tsumo M2 M4 P5 P5 P6 P6 P7 P7 S9 S9 S9 F F M3)     ; iipeikou
    (east east tsumo M2 M3 M4 P1 P1 P1 P2 P3 P4 S2 S3 S4 S S)     ; sanshoku doujun
    (east east tsumo (open M2 M3 M4) P1 P1 P1 P2 P3 P4 S2 S3 S4 S S)  ; sanshoku doujun (open)
    (east east tsumo M9 M9 M9 S2 S3 S4 S5 S6 S7 S8 S9 B B S1)     ; ittsuu
    (east east tsumo (open M9 M9 M9) S2 S3 S4 S5 S6 S7 S8 S9 B B S1)  ; ittsuu (open)
    (east east tsumo M5 M5 P7 P7 P8 P9 P9 S7 S7 S8 S8 S9 S9 P8)     ; ryanpeikou
    (east east tsumo M8 M8 M8 (open P3 P3 P3) S1 S1 S1 S7 S7 N N S7)  ; toitoi
    (east east tsumo M1 M4 M4 M4 P1 P2 P3 P9 P9 P9 S2 S2 S2 M1)     ; san ankou
    (east east tsumo M1 M4 M4 M4 (open P1 P1 P1) P9 P9 P9 S2 S2 S2 M1)  ; san ankou (open)
    (east east tsumo M3 M3 P3 P3 P3 P6 P7 P8 S3 S3 S3 W W M3)     ; sanshoku doukou
    (east east tsumo M3 M3 (open P3 P3 P3) P6 P7 P8 S3 S3 S3 W W M3)  ; sanshoku doukou (open)
    (east east tsumo (closed P1 P1 P1 P1) (closed S2 S2 S2 S2)      ; san kantsu
     (closed M3 M3 M3 M3) S7 S8 S9 W W)
    (east east tsumo (open P1 P1 P1 P1) (closed S2 S2 S2 S2)      ; san kantsu (open)
     (open M3 M3 M3 M3) S7 S8 S9 W W)
    (east east tsumo M2 M3 M4 P2 P2 P2 P6 P7 P8 S5 S6 S7 S8 S5)     ; tanyao
    (east east tsumo M5 M5 P1 P2 P3 P5 P6 P7 C C C S8 S8 S8)      ; fanpai
    (east east tsumo M5 M5 P1 P2 P3 P5 P6 P7 (open C C C) S8 S8 S8)   ; fanpai (open)
    (east east tsumo (open C C C) F F F P1 P2 P3 P4 P5 P6 S1 S1)    ; fanpai (1 open, 1 closed)
    (east east tsumo M7 M8 M9 P1 P1 P1 P7 P8 S9 S9 S9 B B P9)     ; chanta
    (east east tsumo (open M7 M8 M9) P1 P1 P1 P7 P8 S9 S9 S9 B B P9)  ; chanta (open)
    (east east tsumo M1 M2 M3 M9 M9 M9 P9 P9 S1 S2 S7 S8 S9 S3)     ; junchan
    (east east tsumo (open M1 M2 M3) M9 M9 M9 P9 P9 S1 S2 S7 S8 S9 S3)  ; junchan (open)
    (east east tsumo M1 M1 M1 P9 P9 P9 S1 S1 S9 S9 S9 F F S1)     ; toitoi honroutou
    (east east tsumo (open M1 M1 M1) P9 P9 P9 S1 S1 S9 S9 S9 F F S1)  ; toitoi honroutou (open)
    (east east tsumo M1 M1 P1 P1 S9 S9 E S S B B C C E)         ; chiitoitsu honroutou
    (east east tsumo P2 P3 P4 S6 S7 S8 B B F F F C C B)         ; shousangen
    (east east tsumo P2 P3 P4 S6 S7 S8 B B (open F F F) C C B)      ; shousangen (open)
    (east east tsumo P1 P1 P1 P3 P4 P7 P8 P9 S S B B B P5)        ; honitsu
    (east east tsumo (open P1 P1 P1) P3 P4 P7 P8 P9 S S B B B P5)   ; honitsu (open)
    (east east tsumo S1 S3 S4 S4 S4 S5 S5 S6 S6 S6 S7 S8 S9 S2)     ; chinitsu
    (east east tsumo S1 S3 (open S4 S4 S4) S5 S5 S6 S6 S6 S7 S8 S9 S2)  ; chinitsu (open)
    (east east tsumo M1 M9 P1 P9 S1 S9 E S S W N B F C)         ; kokushi musou
    (east east tsumo M1 M9 P1 P9 S1 S9 E S W N B F C S)         ; kokushi musou (pair wait)
    (east east tsumo M4 M4 M4 M8 M8 P9 P9 P9 S2 S2 S2 E E M8)     ; suu ankou
    (east east tsumo M5 M6 M7 S4 S4 B B B F F C C C F)          ; daisangen
    (east east tsumo M5 M6 M7 S4 S4 (open B B B) F F C C C F)     ; daisangen (open)
    (east east tsumo S6 S7 S8 E E E S S S W W N N W)          ; shousuushii
    (east east tsumo S6 S7 S8 (open E E E) S S S W W N N W)       ; shousuushii (open)
    (east east tsumo P3 P3 E E E S S S W W W N N N)           ; daisuushii
    (east east tsumo P3 P3 (open E E E) S S S W W W N N N)        ; daisuushii
    (east east tsumo E E S S S W W B B B F F F E)           ; tsuuiisou
    (east east tsumo E E (open S S S) W W B B B F F F E)        ; tsuuiisou (open)
    (east east tsumo M1 M1 P1 P1 P1 P9 P9 P9 S1 S1 S9 S9 S9 M1)     ; chinroutou
    (east east tsumo M1 M1 (open P1 P1 P1) P9 P9 P9 S1 S1 S9 S9 S9 M1)  ; chinroutou (open)
    (east east tsumo S2 S2 S3 S3 S4 S4 S6 S6 S8 S8 S8 F F S6)     ; ryuuiisou
    (east east tsumo S2 S2 S3 S3 S4 S4 S6 S6 (open S8 S8 S8) F F S6)  ; ryuuiisou (open)
    (east east tsumo P1 P1 P1 P2 P3 P4 P5 P6 P7 P8 P9 P9 P9 P4)     ; chuuren poutou
    (east east tsumo (closed P1 P1 P1 P1) (closed P2 P2 P2 P2)      ; suu kantsu
     (closed S1 S1 S1 S1) (closed S2 S2 S2 S2) M5 M5)
    (east east tsumo (open P1 P1 P1 P1) (open P2 P2 P2 P2)        ; suu kantsu (open)
     (open S1 S1 S1 S1) (closed S2 S2 S2 S2) M5 M5)
    ))

(defparameter *fixed-old-test-hands* (loop for old-hand in *old-test-hands*
                                           collect (list* (pop old-hand)
                                                          (pop old-hand)
                                                          nil
                                                          (pop old-hand)
                                                          old-hand)))

(defparameter *test-hands*
  '((east north (S6) ron B B B C C W W N N N S S S W)
    (south west (M2 M6) tsumo F C C C (closed S7 S7 S7 S7) (open W W W) (open B B B) F)
    (south south (P4) (S9) ron M3 M4 M5 P3 P4 P5 S2 S3 S3 S3 S4 S4 S5 S3)
    (south south (S3) ron M1 M1 M1 B B F F (open C C C) (open S S S) B)
    (south east (P5) ron M1 M1 M1 M2 M3 M4 M5 M6 M7 M8 M9 M9 M9 M5)
    (east west (E S6 M2 P4) ron M1 E E E (open M3 M3 M3 M3) (open C C C C) (open S9 S9 S9 S9) M1)
    ))

(defun format-hans (hans)
  (loop with formatted = (list)
        with prev = nil
        for han in hans
        if (not (equal prev han))
          do (let* ((name (first han))
                    (value (second han))
                    (numeric (numberp value))
                    (repeats (count han hans)))
               (push (format nil "~d x ~a (~:[~a~;~d han~])"
                             repeats name numeric (if numeric (* repeats value) value)) formatted)
               (setf prev han))
        finally (return (nreverse formatted))))


(define-condition no-yaku (error) ())

(defun format-scoring (hand scoring)
  (flet ((r (payment) (round-up-to 100 payment)))
    (let* ((ord (scoring-ord scoring))
           (han (scoring-han scoring))
           (fu (scoring-fu scoring))
           (limit (limit han fu))
           (han-fu (if limit
                       (format nil "~a (~:[~*~;~d han ~]~d fu)" limit (numberp han) han fu)
                       (format nil "~d han ~d fu" han fu)))
           (east-win (equal :east (hand-seat-wind hand)))
           (self-draw (hand-self-draw hand))
           (other (payment hand scoring))
           (east (* 2 other))
           (payments (cond ((and east-win self-draw) (format nil "Everyone pays ~d." (r east)))
                           (self-draw (format nil "East pays ~d, others pay ~d." (r east) (r other)))
                           (t (format nil "Discarder pays ~d." (r (cond (east-win (* 3 east))
                                                                        (t (+ east (* 2 other)))))))))
           (output (format nil "~a~%~@(~a~)~%~{~&~(~a~)~}~%~a" (mapcar #'format-set ord) han-fu
                           (format-hans (scoring-yakus scoring)) payments)))
      (when (equal 0 han) (restart-case (error 'no-yaku)
                            (return-text (text) (setf output text))))
      output)))

(defun scoring-add-dora-han (scoring hits type)
  (scoring-add-han scoring hits (make-list hits :initial-element (list type 1))))

(defparameter *limit-points* '(
                               :kazoe-mangan 2000
                               :mangan 2000
                               :haneman 3000
                               :baiman 4000
                               :sanbaiman 6000
                               :kazoe-yakuman 8000
                               :yakuman 8000
                               :double-yakuman 16000
                               ))

(defun round-up-to (up-to x)
  (* up-to (ceiling x up-to)))

(defun payment (hand scoring)
  (let* ((han (scoring-han scoring))
         (fu (scoring-fu scoring))
         (limit (limit han fu))
         (base (cond
                 (limit (getf *limit-points* limit))
                 (t (* fu (expt 2 (+ 2 han)))))))
    base))


(defun score (hand)
  (let* ((scorings (eval-all-orderings hand))
         (best (reduce #'higher-scoring scorings))
         (tiles (hand-tiles hand))
         (dora-hits (count-doras-hits (hand-doras hand) tiles))
         (ura-dora-hits (if (hand-riichi hand) (count-doras-hits (hand-ura-doras hand) tiles) 0)))
    (when (not (equal 0 (scoring-han best)))
      (setf best (scoring-add-dora-han best dora-hits 'dora))
      (setf best (scoring-add-dora-han best ura-dora-hits 'ura-dora)))
    best))


(defun replace-char-with-str (needle str haystack)
  (with-output-to-string (result)
    (loop for c across haystack
          do (if (char= c needle)
                 (write-string str result)
                 (write-char c result)))))


(defun return-text (text)
  (invoke-restart 'return-text text))

(defun read-parse-and-score (line)
  (restart-case
      (let* ((hand-str (format nil "(~a)"
                               (substitute #\) #\] ; weird parameter order
                                           (replace-char-with-str #\[ "(closed " line))))
             (raw-hand (handler-case (with-input-from-string (in hand-str) (read in))
                         (end-of-file (c) (error 'invalid-hand :reason "Syntax error."))))
             (hand (parse-hand raw-hand))
             (scoring (score hand)))
        (format-scoring hand scoring))
    (return-text (text) text)))


(defun terminate (c)
  (invoke-restart 'terminate))

(defun main ()
  (handler-bind
      ((invalid-hand (lambda (c) (return-text (format nil "Invalid hand: ~a" (reason c)))))
       (no-yaku (lambda (c) (return-text (format nil "No yaku."))))
       (end-of-file #'terminate)
       (error (lambda (c) (return-text (format nil "Unexpected error: ~a" c)))))
    (loop for line = (restart-case (read-line)
                       (use-value (value) value)
                       (terminate () (return nil)))
          if (plusp (length line)) do (format t "~&~a~%" (read-parse-and-score line)))))

(defun test-hands ()
  (loop for raw-hand in (subseq *test-hands* 0)
        for hand = (parse-hand raw-hand)
        do (format t "~a~%~%" (format-scoring hand (score hand)))))
