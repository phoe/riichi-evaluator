
; Suits: :circle :bamboo :character
; Ranks: Numbers 1-9


(defun string-empty-p (str)
  (string= str ""))

(defun cut (str)
  (let ((first-space (position #\Space str)))
	(if (not first-space)
	  (values str "")
	  (values (subseq str 0 first-space) (subseq str (1+ first-space))))))

(defun words (str)
  (multiple-value-bind (first rest) (cut str)
	(if (string-empty-p first)
	  (if (string-empty-p rest)
		(list)
		(words rest))
	  (cons first (words rest)))))


(defparameter *suit-table*
  '((#\P :circle)
	(#\S :bamboo)
	(#\M :character)))

(defparameter *honor-table*
  '((#\E :east)
	(#\S :south)
	(#\W :west)
	(#\N :north)
	(#\C :red)
	(#\F :green)
	(#\B :white)))


(define-condition a-not-in-table (error) ((a :initarg :a)))
(define-condition b-not-in-table (error) ((b :initarg :b)))

(defun table-a-with-b (table key)
  (or
	(loop for (a b) in table
		  when (equal b key) return a)
	(error 'b-not-in-table :b key)))

(defun table-b-with-a (table key)
  (or
	(loop for (a b) in table
		  when (equal a key) return b)
	(error 'a-not-in-table :a key)))

(defun position-b (key table)
  (position-if (lambda (pair) (equal key (second pair))) table))


(defclass tile ()
  ())

(defgeneric simple-p (tile))

(defgeneric terminal-p (tile))

(defgeneric honor-p (tile))

(defgeneric of-suit (suit tile))

(defun lambda-of-suit (suit)
  (lambda (tile) (of-suit suit tile)))

(defgeneric of-rank (rank tile))

(defgeneric tile-equal (tile tile))

(defgeneric tile-less (tile tile))

(defgeneric tile-consec (tile &rest rest))


(defmethod simple-p ((tile tile))
  nil)

(defmethod terminal-p ((tile tile))
  nil)

(defmethod honor-p ((tile tile))
  nil)

(defmethod of-suit (suit (tile tile))
  nil)

(defmethod of-rank (rank (tile tile))
  nil)

(defmethod tile-equal ((a tile) (b tile))
  nil)

(defmethod tile-consec ((tile tile) &rest rest)
  nil)


(defclass suited-tile (tile)
  ((suit :initarg :suit
		 :reader suit)
   (rank :initarg :rank
		 :reader rank)))

(defmethod print-object ((tile suited-tile) stream)
  (when *print-readably* (error 'print-not-readable :object tile))
  (format stream "#<~C~d>"
		  (table-a-with-b *suit-table* (suit tile))
		  (rank tile)))

(defmethod simple-p ((tile suited-tile))
  (case (rank tile)
	((1 9)     nil)
	(otherwise t)))

(defmethod terminal-p ((tile suited-tile))
  (not (simple-p tile)))

(defmethod of-suit (suit (tile suited-tile))
  (equal suit (suit tile)))

(defmethod of-rank (rank (tile suited-tile))
  (equal rank (rank tile)))

(defmethod tile-equal ((a suited-tile) (b suited-tile))
  (and (of-rank (rank a) b)
	   (of-suit (suit a) b)))

(defmethod tile-less ((a suited-tile) (b suited-tile))
  (let ((pos-a (position-b (suit a) *suit-table*))
		(pos-b (position-b (suit b) *suit-table*)))
	(cond ((< pos-a pos-b) t)
		  ((= pos-a pos-b) (< (rank a) (rank b)))
		  (t               nil))))

(defmethod tile-consec ((tile suited-tile) &rest rest)
  ;(format t "Called: ~a ~a~%" tile rest)
  (case (length rest)
	(0 t)
	(1 (and (of-suit (suit tile) (first rest))
			(of-rank (1+ (rank tile)) (first rest))))
	(otherwise (and (tile-consec tile (first rest))
					(apply #'tile-consec (first rest) (rest rest))))))


(defclass honor-tile (tile)
  ((kind :initarg :kind
		 :reader kind)))

(defmethod print-object ((tile honor-tile) stream)
  (when *print-readably* (error 'print-not-readable :object tile))
  (format stream "#<~C>"
		  (table-a-with-b *honor-table* (kind tile))))

(defmethod honor-p ((tile honor-tile))
  t)

(defmethod tile-equal ((a honor-tile) (b honor-tile))
  (equal (kind a) (kind b)))

(defmethod tile-less ((a honor-tile) (b honor-tile))
  (let ((pos-a (position-b (kind a) *honor-table*))
		(pos-b (position-b (kind b) *honor-table*)))
	(< pos-a pos-b)))


(defmethod tile-less ((a suited-tile) (b honor-tile)) t)
(defmethod tile-less ((a honor-tile) (b suited-tile)) nil)


(define-condition parse-tile-error (error)
  ((text :initarg :text
		 :reader text)))

(defun parse-tile (text)
  (case (length text)
	(2 (make-instance 'suited-tile
					  :suit (table-b-with-a *suit-table* (char text 0))
					  :rank (or
							  (digit-char-p (char text 1))
							  (error 'parse-tile-error :text text))))
	(1 (make-instance 'honor-tile
					  :kind (table-b-with-a *honor-table* (char text 0))))
	(otherwise (error 'parse-tile-error :text text))))

(defun parse-hand (text)
  (sort (loop for word in (words text)
			  collect (parse-tile word))
		#'tile-less))


(defparameter *matchers* (list))


(defun match-and-consume (matcher tiles)
  ;(format t "Matching: ~a ~a~%" matcher tiles)
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
		;(format t "Remainder-paths: ~a~%" remainder-paths)
		(add-to-paths pattern remainder-paths)))))


(defun sub-paths (tiles)
  (when tiles
	(loop for m in *matchers*
		  if (match-recursive m tiles)
		  nconc it)))



;(add-to-paths 1 (list))
;(add-to-paths 0 (add-to-paths 1 (nconc (add-to-paths 2 nil) (add-to-paths 'b nil))))


(defmacro pat-type (pat)
  `(first ,pat))


(defun lambda-of-type (&rest types)
  (lambda (pat) (find (pat-type pat) types)))


(defun ready-sets (ord)
  (remove-if (lambda-of-type 'single) ord)) 


(define-condition unknown-kind-of-set (error)
  ((kind :initarg kind)))

(defmacro with-sets (ord &rest forms)
  `(let* ((ready-sets (ready-sets ,ord))
		  (closed-sets ready-sets)
		  (open-sets (list)))
	 (flet ((sets (kind &rest types)
				  (remove-if-not (apply #'lambda-of-type types)
								 (case kind
								   (:closed closed-sets)
								   (:open open-sets)
								   (:all ready-sets)
								   (otherwise (error 'unknown-kind-of-set :kind kind))))))
	   ,@forms)))


(defparameter *yakus* (list))

(defmacro define-yaku (name han-open han-closed &rest forms)
  `(push
	 (lambda (ord)
	   (with-sets ord 
				  (when (progn ,@forms)
					(list ',name))))
	 *yakus*))


(defun ordering-ready-p (ord)
  (with-sets ord
			 (or 
			   (and (= 4 (+ (length (sets :all 'chi 'pon 'kan))
							(* 3 (length (sets :all 'ittsuu)))))
					(= 1 (length (sets :all 'pair))))
			   (= 7 (length (sets :all 'pair))))))


(defun eval-ordering (ord)
  (loop for yaku in *yakus*
		if (funcall yaku ord)
		nconc it))


(defun eval-all-orderings (hand)
  (loop for ordering in (sub-paths hand)
		if (ordering-ready-p ordering)
		collect (eval-ordering ordering)))


; A pattern-matcher returns two values:
; pat: a matched pattern or nil if no match
; new-tiles: a list of remaining tiles or amount of tiles to be consumed
(defmacro define-pattern (name min-length &rest forms)
  `(push (lambda (tiles)
		   (when (>= (length tiles) ,min-length)
			 ,@forms))
		 *matchers*))


(defun count-consec-sames (tiles)
  (let ((len (length tiles)))
	(case len
	  (0 (error 'no-tiles))
	  (1 1)
	  (otherwise
		(or (position (first tiles) tiles :test-not #'tile-equal)
			len)))))

(defmacro define-sames-pattern (name length)
  `(define-pattern ,name ,length
				   ;(format t "Sames-matcher: ~a ~d~%" ',name ,length)
				   (if (<= ,length (count-consec-sames tiles))
					 (values (cons ',name (subseq tiles 0 ,length)) ,length)
					 (values nil 0))))

;(define-sames-pattern single 1)
(define-sames-pattern pair 2)
(define-sames-pattern pon 3)
(define-sames-pattern kan 4)

(defun remove-nth (n seq)
  (remove-if (lambda (x) t) seq :start n :count 1))

; Returns two values; the second is what was found and removed, nil if none.
(defun remove-if-exists (item seq test)
  (let ((pos (position item seq :test test)))
	(if pos
	  (values (remove-nth pos seq) t)
	  (values seq nil))))

(defun find-straight (tiles max-length)
  (if (and tiles
		   (> max-length 0))
	(let* ((t1 (first tiles))
		   (t2-pos (position-if (lambda (t2) (tile-consec t1 t2)) tiles :start 1))
		   (skipped (if t2-pos (subseq tiles 1 t2-pos) (rest tiles)))
		   (next-tiles (if t2-pos (subseq tiles t2-pos) (list))))
	  (multiple-value-bind (next-found next-skipped) (find-straight next-tiles (1- max-length))
		(values (cons t1 next-found) (nconc skipped next-skipped))))
	(values (list) tiles)))

(defmacro define-straight-pattern (name len)
  `(define-pattern ,name ,len
				   (multiple-value-bind (found skipped) (find-straight tiles ,len)
					 ;(format t "Tiles: ~a~%Found: ~a~%Skipped: ~a~%" tiles found skipped)
					 (if (= ,len (length found))
					   (values (cons ',name found) skipped)
					   (values nil 0)))))

(define-straight-pattern chi 3)
(define-straight-pattern ittsuu 9)


(define-yaku chiitoitsu 0 2
			 (= 7 (length (sets :all 'pair))))


(defun chi-equal (c1 c2)
  (tile-equal (second c1) (second c2)))


(defun count-chi-pairs (chis)
  (if chis
	(let* ((c1 (first chis))
		   (rest-chis (rest chis))
		   (c2-pos (position-if (lambda (c2) (chi-equal c1 c2)) rest-chis))
		   (next-chis (if c2-pos
						(remove-if (lambda (x) t) rest-chis :start c2-pos :count 1)
						rest-chis)))
	  ;(when c2-pos
	  ;  (format t "Matching chi for ~a found at ~d.~%" c1 c2-pos))
	  (+ (if c2-pos 1 0) (count-chi-pairs next-chis)))
	0))

(define-yaku iipeikou 0 1
			 (= 1 (count-chi-pairs (sets :closed 'chi))))

(defun set-rank (chi)
  (rank (second chi)))

(defun set-suit (chi)
  (suit (second chi)))

(defun set-equal-rank (c1 c2)
  (= (set-rank c1) (set-rank c2)))

(defun sanshoku (sets)
  (let ((len (length sets)))
	(when (>= 3 len)
	  (loop for i from 0 upto (- len 3)
					   for i-set = (elt sets i)
					   for i-rank = (set-rank i-set)
					   for suits = (loop for j from (1+ i) upto (1- len)
										 for j-set = (elt sets j)
										 for j-rank = (set-rank j-set)
										 if (= i-rank j-rank)
										 collect (set-suit j-set))
					   for diff-suits = (delete-duplicates suits)
					   thereis (= 2 (length diff-suits))))))

(define-yaku sanshoku-doujun 1 2
			 (sanshoku (sets :all 'chi)))

;			 (let* ((chis (sets :all 'chi))
;					(len (length chis)))
;			   (when (>= 3 len)
;				 (loop for i from 0 upto (- len 3)
;					   for i-chi = (elt chis i)
;					   for i-rank = (set-rank i-chi)
;					   for suits = (loop for j from (1+ i) upto (1- len)
;										 for j-chi = (elt chis j)
;										 for j-rank = (set-rank j-chi)
;										 if (= i-rank j-rank)
;										 collect (set-suit j-chi))
;					   for diff-suits = (delete-duplicates suits)
;					   thereis (= 2 (length diff-suits))))))

(define-yaku ittsuu 1 2
			 (sets :all 'ittsuu))

(define-yaku ryanpeikou 0 3
			 (= 2 (count-chi-pairs (sets :closed 'chi))))

(define-yaku toitoi 2 2
			 (= 4 (length (sets :all 'pon 'kan))))

(define-yaku san-ankou 2 2
			 (= 3 (length (sets :closed 'pon 'kan))))

(define-yaku sanshoku-doukou 2 2
			 (sanshoku (sets :all 'pon)))












;(defparameter *hand* (parse-hand "P1 P2 P3 P7"))
;(defparameter *hand* (parse-hand "W P1 S4 P5 P5 P1 C S9 M3 E M6 M8 M7 P5"))
;(defparameter *paths* (sub-paths *hand*))
;(defparameter *ord* (first (sub-paths *hand*)))

;*hand*

;(print *paths*)

(defparameter *test-hands*
  '("M3 M3 P1 P1 P5 P5 S1 S1 S8 E E C C S8"
	"M1 M2 M3 M5 M5 P3 P4 P7 P8 P9 S4 S5 S6 P2"
	"M2 M4 P5 P5 P6 P6 P7 P7 S9 S9 S9 F F M3"
	"M2 M3 M4 P1 P1 P1 P2 P3 P4 S2 S3 S4 S S"
	"M9 M9 M9 S2 S3 S4 S5 S6 S7 S8 S9 B B S1"
	"M5 M5 P7 P7 P8 P9 P9 S7 S7 S8 S8 S9 S9 P8"
	"M8 M8 M8 P3 P3 P3 S1 S1 S1 S7 S7 N N S7"
	"M1 M4 M4 M4 P1 P2 P3 P9 P9 P9 S2 S2 S2 M1"
	"M3 M3 P3 P3 P3 P6 P7 P8 S3 S3 S3 W W M3"
	"M2 M3 M4 P2 P2 P2 P6 P7 P8 S5 S6 S7 S8 S5"
	))

(loop for raw-hand in *test-hands* ; (subseq *test-hands* 4 5)
	  for hand = (parse-hand raw-hand)
	  ;do (format t "~a~%" (sub-paths hand))
	  do (format t "Hand: ~a~%Interpretations: ~{~:a ~}~%~%" hand (eval-all-orderings hand)))

;(print (eval-all-orderings *hand*))


;(print *hand*)

;(sub-paths *hand*)

;(print (eval-all-orderings *hand*))

;(mapcar (lambda-of-suit :character) (parse-hand "P1 S2 M5"))

;(format nil "~a" (parse-tile "P1"))

