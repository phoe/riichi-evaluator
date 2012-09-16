
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

(defparameter *wind-table*
  '((#\E :east)
	(#\S :south)
	(#\W :west)
	(#\N :north)))

(defparameter *dragon-table*
  '((#\C :chuu)
	(#\B :haku)
	(#\F :hatsu)))

(defparameter *honor-table*
  (append *wind-table* *dragon-table*))


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

(defgeneric tile-p (t))

(defgeneric suited-p (tile))

(defgeneric simple-p (tile))

(defgeneric terminal-p (tile))

(defgeneric honor-p (tile))

(defgeneric wind-p (tile))

(defgeneric dragon-p (tile))

(defgeneric of-suit (suit tile))

(defun lambda-of-suit (suit)
  (lambda (tile) (of-suit suit tile)))

(defgeneric of-rank (rank tile))

(defgeneric of-wind (wind tile))

(defgeneric tile-equal (tile1 tile2))

(defgeneric tile-less (tile1 tile2))

(defgeneric tile-consec (tile &rest rest))


(defmethod tile-p ((anything t))
  nil)

(defmethod tile-p ((tile tile))
  t)

(defmethod suited-p ((tile tile))
  nil)

(defmethod simple-p ((tile tile))
  nil)

(defmethod terminal-p ((tile tile))
  nil)

(defmethod honor-p ((tile tile))
  nil)

(defmethod wind-p ((tile tile))
  nil)

(defmethod dragon-p ((tile tile))
  nil)

(defmethod of-suit (suit (tile tile))
  nil)

(defmethod of-rank (rank (tile tile))
  nil)

(defmethod of-wind (wind (tile tile))
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

(defmethod suited-p ((tile suited-tile))
  t)

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

(defmethod wind-p ((tile honor-tile))
  (position-b (kind tile) *wind-table*))

(defmethod dragon-p ((tile honor-tile))
  (position-b (kind tile) *dragon-table*))

(defmethod of-wind (wind (tile honor-tile))
  (equal wind (kind tile)))

(defmethod tile-equal ((a honor-tile) (b honor-tile))
  (equal (kind a) (kind b)))

(defmethod tile-less ((a honor-tile) (b honor-tile))
  (let ((pos-a (position-b (kind a) *honor-table*))
		(pos-b (position-b (kind b) *honor-table*)))
	(< pos-a pos-b)))


(defmethod tile-less ((a suited-tile) (b honor-tile)) t)
(defmethod tile-less ((a honor-tile) (b suited-tile)) nil)


(defstruct hand
  prevailing-wind
  seat-wind
  self-draw
  winning-tile
  tiles
  locked-sets
  free-tiles
  closed)


(define-condition parse-tile-error (error)
  ((text :initarg :text
		 :reader text)))

(defun parse-tile (symbol)
  (let ((text (string symbol)))
	(case (length text)
	  (2 (make-instance 'suited-tile
						:suit (table-b-with-a *suit-table* (char text 0))
						:rank (or
								(digit-char-p (char text 1))
								(error 'parse-tile-error :text text))))
	  (1 (make-instance 'honor-tile
						:kind (table-b-with-a *honor-table* (char text 0))))
	  (otherwise (error 'parse-tile-error :text text)))))


(defun parse-text-hand (text)
  (sort (loop for word in (words text)
			  collect (parse-tile word))
		#'tile-less))


(defun parse-set (seq)
  (let* ((open-or-closed (ecase (first seq)
						   ('open :open)
						   ('closed :closed)))
		 (tiles (mapcar #'parse-tile (rest seq)))
		 (type (ecase (length tiles)
				 (2 'pair)
				 (3 (if (tile-equal (first tiles) (second tiles)) 'pon 'chi))
				 (4 'kan))))
	(append (list open-or-closed type) tiles)))


(defun parse-hand (seq)
  (flet ((parse-wind (symbol) (ccase symbol
									 ('east :east)
									 ('south :south)
									 ('west :west)
									 ('north :north))))
	(let ((prevailing-wind (parse-wind (pop seq)))
		  (seat-wind (parse-wind (pop seq)))
		  (self-draw (ecase (pop seq)
					   ('tsumo t)
					   ('ron nil))))
	  (loop for item in seq
			if (listp item) collect (parse-set item) into locked-sets
			if (symbolp item) collect (parse-tile item) into free-tiles
			finally (let* ((tiles-in-locked-sets (mapcan #'set-tiles locked-sets))
						   (winning-tile (car (last free-tiles)))
						   (tiles (append free-tiles tiles-in-locked-sets))
						   (closed (all #'set-closed-p locked-sets)))
					  (return (make-hand :prevailing-wind prevailing-wind
										 :seat-wind seat-wind
										 :self-draw self-draw
										 :winning-tile winning-tile
										 :tiles (sort tiles #'tile-less)
										 :locked-sets locked-sets
										 :free-tiles (sort free-tiles #'tile-less)
										 :closed closed)))))))


(defparameter *pattern-matchers* (list))


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
	(loop for m in *pattern-matchers*
		  if (match-recursive m tiles)
		  nconc it)))



;(add-to-paths 1 (list))
;(add-to-paths 0 (add-to-paths 1 (nconc (add-to-paths 2 nil) (add-to-paths 'b nil))))


(define-condition unknown-kind-of-set (error)
  ((kind :initarg kind)))

(defmacro set-open-closed (set)
  `(first ,set))

(defun set-open-closed-p (open-closed set)
  (if (equal open-closed :all)
	t
	(equal open-closed (first set))))

(defun set-closed-p (set)
  (equal :closed (first set)))

(defun set-type (set)
  (second set))

(defun set-kan-p (set)
  (equal 'kan (set-type set)))


(defmacro with-hand-helpers (hand-name ord-name &rest forms)
  `(let* ((prevailing-wind (hand-prevailing-wind ,hand-name))
		  (seat-wind (hand-seat-wind ,hand-name))
		  (self-draw (hand-self-draw ,hand-name))
		  (winning-tile (hand-winning-tile ,hand-name))
		  (winning-set (find (lambda (set) (find winning-tile (set-tiles set))) ,ord-name))
		  (tiles (hand-tiles ,hand-name))
		  (closed (hand-closed ,hand-name)))

	 (labels ((lambda-p (open-closed types)
						(lambda (set) (and (set-open-closed-p open-closed set)
										   (find (set-type set) types))))
			  (sets (open-closed &rest types) (remove-if-not (lambda-p open-closed types) ,ord-name))
			  (count-sets (open-closed &rest types) (count-if (lambda-p open-closed types) ,ord-name))
			  (of-good-wind-p (tile) (or (of-wind prevailing-wind tile) (of-wind seat-wind tile))))
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
	('pair 2) ; pair wait
	('chi (let* ((tiles (set-tiles winning-set))
				 (pos (position winning-tile tiles))
				 (rank (set-rank winning-set)))
			(ecase pos
			  (2 (if (= rank 1) 2 0))    ; edge wait     1   2  <3>
			  (1 2) 				     ; closed wait
			  (0 (if (= rank 7) 2 0))))) ; edge wait    <7>  8   9
	(otherwise 0)))



(defun count-fu (hand ord)
  (with-hand-helpers hand ord
					 (let* ((pairs (sets :all 'pair))
							(seven-pairs (= 7 (length pairs)))
							(for-melds (count-fu-melds (sets :all 'pon 'kan)))
							(for-pairs (count-fu-pairs pairs #'of-good-wind-p))
							(for-waits (count-fu-waits winning-set winning-tile))
							(for-all-of-above (+ for-melds for-pairs for-waits))
							(pinfu-p (and (zerop for-all-of-above) (not seven-pairs)))
							(for-self-draw (if (and self-draw (not pinfu-p)) 2 0))
							(for-open-pinfu (if (not closed) 2 0))
							(base-total (+ for-all-of-above for-self-draw for-open-pinfu))
							(for-winning (cond (seven-pairs 25)
											   (self-draw 20)
											   (t 30)))
							(total (+ base-total for-winning)))
					   (values (* 10 (ceiling total 10))
							   base-total))))



(defun qualify-ord (ord)
  (mapcar (lambda (set) (cons :closed set)) ord))

(defun full-ord (path locked-sets)
  (append locked-sets path))



(defmacro yakulist-han (yakulist)
  `(first ,yakulist))

(defmacro yakulist-yakus (yakulist)
  `(rest ,yakulist))


(defun either (a b target)
  (or (equal a target) (equal b target)))

(defun combine-han (a b)
  (cond
	((either a b :double-yakuman) :double-yakuman)
	((either a b :yakuman) :yakuman)
	(t (+ a b))))

(defun combine-yakulists (&optional a b)
  (if (not a)
	(list 0)
	(let ((new-han (combine-han (yakulist-han a) (yakulist-han b)))
		  (new-yakus (nconc (yakulist-yakus a) (yakulist-yakus b))))
	  (cons new-han new-yakus))))


(defmacro scoring-han (scoring)
  `(first ,scoring))

(defmacro scoring-fu (scoring)
  `(second ,scoring))

(defmacro scoring-yakus (scoring)
  `(cddr ,scoring))

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



(defparameter *yaku-matchers* (list))
(defparameter *yaku-list* (list))


(defun limit (han fu)
  (case han
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


(defun eval-ordering (hand ord)
  ;(format t "~a~%" ord)
  (multiple-value-bind (fu base-fu) (count-fu hand ord)
	;(format t "Base-fu: ~a, Fu: ~a~%" base-fu fu)
	(let* ((all-yakus (loop for yaku-p in *yaku-matchers*
							if (funcall yaku-p hand ord base-fu)
							nconc it))
		   (combined (reduce #'combine-yakulists all-yakus)))
	  (list* (first combined) fu (rest combined)))))



(defun ordering-ready-p (hand ord)
  (with-hand-helpers hand ord
					 (let ((chi-pon-kan (+ (count-sets :all 'chi 'pon 'kan)
										   (* 3 (count-sets :all 'ittsuu))))
						   (pair (count-sets :all 'pair)))
					   (or
						 (and (= 4 chi-pon-kan)
							  (= 1 pair))
						 (= 7 pair)
						 (and (= 1 (count-sets :all 'twelve-singles))
							  (= 1 pair)
							  (not (any #'simple-p tiles)))))))


(defun consider-winning-tile (hand unlocked-sets ord)
  (let* ((winning-tile (hand-winning-tile hand))
		 (self-draw (hand-self-draw hand))
		 (possible-winning
		   (loop for set in unlocked-sets
				 for tile = (find winning-tile (set-tiles set) :test #'tile-equal)
				 if tile collect (list set tile)))
		 )
	(loop for (win-set win-tile) in possible-winning
		  for alt-hand = (copy-hand hand)
		  for alt-ord = (loop for set in ord
							  for alt-set = (copy-list set)
							  if (and (not self-draw)
									  (eq win-set set)) do (setf (set-open-closed alt-set) :open)
							  collect alt-set)
		  do (setf (hand-winning-tile alt-hand) win-tile)
		  collect (list alt-hand alt-ord win-set))))


(defun eval-all-orderings (hand)
  (let ((locked-sets (hand-locked-sets hand)))
	(loop for partial-ord in (sub-paths (hand-free-tiles hand))
		  for qualified-partial-ord = (qualify-ord partial-ord)
		  for full-ord = (full-ord qualified-partial-ord locked-sets)
		  if (ordering-ready-p hand full-ord)
		  nconc
		  ; If there are multiple free tiles equal to the winning tile
		  ; all of them should be considered as the winning tile in turn.
		  (loop for (alt-hand alt-ord) in (consider-winning-tile
											hand qualified-partial-ord full-ord)
				collect (eval-ordering alt-hand alt-ord)))))


(defun add-yaku-to-list (name han-closed han-open)
  (push (list name han-closed han-open) *yaku-list*))


(defmacro define-yaku (name han-closed han-open &rest forms)
  `(progn
	 (push
	   (lambda (hand ord base-fu)
		 (with-hand-helpers hand ord
							(when (progn ,@forms)
							  (let ((han (if closed ,han-closed ,han-open)))
								(unless (equal han 0)
								  (list (list han ',name)))))))
	   *yaku-matchers*)
	 (add-yaku-to-list ',name ,han-closed ,han-open)))


(defmacro define-multi-yaku (&rest forms)
  `(push
	 (lambda (hand ord base-fu)
	   (remove nil (with-hand-helpers hand ord
									  ,@forms)))
	 *yaku-matchers*))


(defun yakuman-p (yaku)
  (find :yakuman yaku))

(defun double-yakuman-p (yaku)
  (find :double-yakuman yaku))


; A pattern-matcher returns two values:
; pat: a matched pattern or nil if no match
; new-tiles: a list of remaining tiles or amount of tiles to be consumed
(defmacro define-pattern (name min-length &rest forms)
  `(push (lambda (tiles)
		   (when (>= (length tiles) ,min-length)
			 ,@forms))
		 *pattern-matchers*))


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

(defun find-matching (predicate tiles max-length)
  (if (and tiles
		   (> max-length 0))
	(let* ((t1 (first tiles))
		   (t2-pos (position-if (lambda (t2) (funcall predicate t1 t2)) tiles :start 1))
		   (skipped (if t2-pos (subseq tiles 1 t2-pos) (rest tiles)))
		   (next-tiles (if t2-pos (subseq tiles t2-pos) (list))))
	  (multiple-value-bind (next-found next-skipped) (find-matching predicate
																	next-tiles
																	(1- max-length))
		(values (cons t1 next-found) (nconc skipped next-skipped))))
	(values (list) tiles)))

(defun find-straight (tiles max-length)
  (find-matching (lambda (t1 t2) (tile-consec t1 t2)) tiles max-length))

(defmacro define-straight-pattern (name len)
  `(define-pattern ,name ,len
				   (multiple-value-bind (found skipped) (find-straight tiles ,len)
					 ;(format t "Tiles: ~a~%Found: ~a~%Skipped: ~a~%" tiles found skipped)
					 (if (= ,len (length found))
					   (values (cons ',name found) skipped)
					   (values nil 0)))))

(define-straight-pattern chi 3)
(define-straight-pattern ittsuu 9)

(define-pattern twelve-singles 12
				(multiple-value-bind (found skipped)
				  (find-matching (lambda (t1 t2) (= 1 (count t2 tiles :test #'tile-equal)))
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
	  ;(when c2-pos
	  ;  (format t "Matching chi for ~a found at ~d.~%" c1 c2-pos))
	  (+ (if c2-pos 1 0) (count-chi-pairs next-chis)))
	0))

(defun set-rank (chi)
  (rank (set-first chi)))

(defun set-suit (chi)
  (suit (set-first chi)))

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
			;do (format t "Suits: ~a, Diff-suits: ~a~%" suits diff-suits)
			thereis (= 2 (length diff-suits))))))

(defun set-tiles (set)
  (copy-list (cddr set)))

(defun set-first (set)
  (third set))

(defun all (predicate seq)
  (loop for item in seq
		always (funcall predicate item)))

(defun any (predicate seq)
  (loop for item in seq
		thereis (funcall predicate item)))

(defmacro all-in-set (predicate set)
  `(all ,predicate (set-tiles ,set)))

(defmacro any-in-set (predicate set)
  `(any ,predicate (set-tiles ,set)))

(defun set-kind (set)
  (kind (set-first set)))

(defun all-of (predicate key amount sets)
  (loop for set in sets
		if (funcall predicate (set-first set)) collect set into type-sets
		finally (return (= amount
						   (length (delete-duplicates type-sets :key key))))))

(defun all-kinds-of-type (predicate amount sets)
  (all-of predicate #'set-kind amount sets))

(defun all-suits-of-rank (rank amount sets)
  (all-of (lambda (tile) (of-rank rank tile)) #'set-suit sets))

(defgeneric green-p (tile))
(defmethod green-p ((tile tile))
  nil)
(defmethod green-p ((tile suited-tile))
  (and (equal :bamboo (suit tile))
	   (find (rank tile) '(2 3 4 6 8))))
(defmethod green-p ((tile honor-tile))
  (equal :hatsu (kind tile)))

(defun all-of-same-suit (tiles)
  (let ((first-suited (find-if #'suited-p tiles)))
	(when first-suited
	  (let* ((suit (suit first-suited))
			 (first-other-suited (find-if-not (lambda-of-suit suit) tiles)))
		(not first-other-suited)))))

(define-yaku menzen-tsumo 1 0
			 (and self-draw closed))

(define-yaku tanyao 1 0
			 (all #'simple-p tiles))

(define-yaku pinfu 1 0
			 (zerop base-fu))

(define-yaku iipeikou 1 0
			 (= 1 (count-chi-pairs (sets :closed 'chi))))

(define-yaku sanshoku-doujun 2 1
			 (sanshoku (sets :all 'chi)))

(define-yaku ittsuu 2 1
			 (sets :all 'ittsuu))

; fanpai
(loop for type in (list* :prevailing-wind :seat-wind (mapcar #'second *dragon-table*))
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
		if type collect (list 1 (intern (format nil "FANPAI-~a" type)))))

(define-yaku chanta 2 1
			 (and (any #'terminal-p tiles)
				  (any #'honor-p tiles)
				  (plusp (count-sets :all 'chi))
				  (all (lambda (set) (not (all #'simple-p (set-tiles set))))
					   (sets :all 'pair 'chi 'pon 'kan))))

(define-yaku chiitoitsu 2 0
			 (= 7 (count-sets :all 'pair)))

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

(define-yaku honitsu 3 2
			 (if (any #'honor-p tiles)
			   (all-of-same-suit (remove-if #'honor-p tiles))))

(define-yaku shousangen 2 2
			 (all-kinds-of-type #'dragon-p 3 (sets :all 'pair 'pon 'kan)))

(define-yaku honroutou 2 2
			 (not (any #'simple-p tiles)))

(define-yaku junchan 3 2
			 (and (plusp (count-sets :all 'chi))
				  (all (lambda (set) (any #'terminal-p (set-tiles set)))
					   (sets :all 'pair 'chi 'pon 'kan))))

(define-yaku ryanpeikou 3 0
			 (= 2 (count-chi-pairs (sets :closed 'chi))))

(define-yaku chinitsu 6 5
			 (all-of-same-suit tiles))

(define-yaku kokushi-musou :yakuman 0
			 (and (not (any #'simple-p tiles))
				  (= 1 (count-sets :all 'twelve-singles))
				  (= 1 (count-sets :all 'pair))))

(define-yaku suu-ankou :yakuman :yakuman
			 (= 4 (count-sets :closed 'pon 'kan)))

(define-yaku daisangen :yakuman :yakuman
			 (all-kinds-of-type #'dragon-p 3 (sets :all 'pon 'kan)))

(define-yaku shousuushii :yakuman :yakuman
			 (all-kinds-of-type #'wind-p 4 (sets :all 'pair 'pon 'kan)))

(define-yaku daisuushii :double-yakuman :double-yakuman
			 (all-kinds-of-type #'wind-p 4 (sets :all 'pon 'kan)))

(define-yaku tsuuiisou :yakuman :yakuman
			 (all #'honor-p tiles))

(define-yaku chinroutou :yakuman :yakuman
			 (all #'terminal-p tiles))

(define-yaku ryuuiisou :yakuman :yakuman
			 (all #'green-p tiles))

(define-yaku chuuren-poutou :yakuman 0
			 (and (all-of-same-suit tiles)
				  (let ((ranks (mapcar #'rank tiles)))
					(and (= 3 (count 1 ranks))
						 (= 3 (count 9 ranks))))))

(define-yaku suu-kantsu :yakuman :yakuman
			 (= 4 (count-sets :all 'kan)))


;(defparameter *hand* (parse-hand "P1 P2 P3 P7"))
;(defparameter *hand* (parse-hand "W P1 S4 P5 P5 P1 C S9 M3 E M6 M8 M7 P5"))
;(defparameter *paths* (sub-paths *hand*))
;(defparameter *ord* (first (sub-paths *hand*)))

;*hand*

;(print *paths*)

(defparameter *test-hands*
  '((east east ron M1 M2 M3 P2 P3 P4 S3 S4 S5 (open M6 M7 M8) P7 P7)	; nothing
	(east east ron M3 M3 P1 P1 P5 P5 S1 S1 S8 E E C C S8)				; chiitoitsu
	(east east tsumo M1 M2 M3 M5 M5 P3 P4 P7 P8 P9 S4 S5 S6 P2)			; pinfu
	(east east tsumo M2 M4 P5 P5 P6 P6 P7 P7 S9 S9 S9 F F M3)			; iipeikou
	(east east tsumo M2 M3 M4 P1 P1 P1 P2 P3 P4 S2 S3 S4 S S)			; sanshoku doujun
	(east east tsumo (open M2 M3 M4) P1 P1 P1 P2 P3 P4 S2 S3 S4 S S)	; sanshoku doujun (open)
	(east east tsumo M9 M9 M9 S2 S3 S4 S5 S6 S7 S8 S9 B B S1)			; ittsuu
	(east east tsumo (open M9 M9 M9) S2 S3 S4 S5 S6 S7 S8 S9 B B S1)	; ittsuu (open)
	(east east tsumo M5 M5 P7 P7 P8 P9 P9 S7 S7 S8 S8 S9 S9 P8)			; ryanpeikou
	(east east tsumo M8 M8 M8 (open P3 P3 P3) S1 S1 S1 S7 S7 N N S7)	; toitoi
	(east east tsumo M1 M4 M4 M4 P1 P2 P3 P9 P9 P9 S2 S2 S2 M1)			; san ankou
	(east east tsumo M1 M4 M4 M4 (open P1 P1 P1) P9 P9 P9 S2 S2 S2 M1)	; san ankou (open)
	(east east tsumo M3 M3 P3 P3 P3 P6 P7 P8 S3 S3 S3 W W M3)			; sanshoku doukou
	(east east tsumo M3 M3 (open P3 P3 P3) P6 P7 P8 S3 S3 S3 W W M3)	; sanshoku doukou (open)
	(east east tsumo (closed P1 P1 P1 P1) (closed S2 S2 S2 S2)			; san kantsu	
		  (closed M3 M3 M3 M3) S7 S8 S9 W W)
	(east east tsumo (open P1 P1 P1 P1) (closed S2 S2 S2 S2)			; san kantsu (open)
		  (open M3 M3 M3 M3) S7 S8 S9 W W)
	(east east tsumo M2 M3 M4 P2 P2 P2 P6 P7 P8 S5 S6 S7 S8 S5)			; tanyao
	(east east tsumo M5 M5 P1 P2 P3 P5 P6 P7 C C C S8 S8 S8)			; fanpai
	(east east tsumo M5 M5 P1 P2 P3 P5 P6 P7 (open C C C) S8 S8 S8)		; fanpai (open)
	(east east tsumo (open C C C) F F F P1 P2 P3 P4 P5 P6 S1 S1)		; fanpai (1 open, 1 closed)
	(east east tsumo M7 M8 M9 P1 P1 P1 P7 P8 S9 S9 S9 B B P9)			; chanta
	(east east tsumo (open M7 M8 M9) P1 P1 P1 P7 P8 S9 S9 S9 B B P9)	; chanta (open)
	(east east tsumo M1 M2 M3 M9 M9 M9 P9 P9 S1 S2 S7 S8 S9 S3)			; junchan
	(east east tsumo (open M1 M2 M3) M9 M9 M9 P9 P9 S1 S2 S7 S8 S9 S3)	; junchan (open)
	(east east tsumo M1 M1 M1 P9 P9 P9 S1 S1 S9 S9 S9 F F S1)			; toitoi honroutou
	(east east tsumo (open M1 M1 M1) P9 P9 P9 S1 S1 S9 S9 S9 F F S1)	; toitoi honroutou (open)
	(east east tsumo M1 M1 P1 P1 S9 S9 E S S B B C C E)					; chiitoitsu honroutou
	(east east tsumo P2 P3 P4 S6 S7 S8 B B F F F C C B)					; shousangen
	(east east tsumo P2 P3 P4 S6 S7 S8 B B (open F F F) C C B)			; shousangen (open)
	(east east tsumo P1 P1 P1 P3 P4 P7 P8 P9 S S B B B P5)				; honitsu
	(east east tsumo (open P1 P1 P1) P3 P4 P7 P8 P9 S S B B B P5)		; honitsu (open)
	(east east tsumo S1 S3 S4 S4 S4 S5 S5 S6 S6 S6 S7 S8 S9 S2)			; chinitsu
	(east east tsumo S1 S3 (open S4 S4 S4) S5 S5 S6 S6 S6 S7 S8 S9 S2)	; chinitsu (open)
	(east east tsumo M1 M9 P1 P9 S1 S9 E S S W N B F C)					; kokushi musou
	(east east tsumo M1 M9 P1 P9 S1 S9 E S W N B F C S)					; kokushi musou (pair wait)
	(east east tsumo M4 M4 M4 M8 M8 P9 P9 P9 S2 S2 S2 E E M8)			; suu ankou
	(east east tsumo M5 M6 M7 S4 S4 B B B F F C C C F)					; daisangen
	(east east tsumo M5 M6 M7 S4 S4 (open B B B) F F C C C F)			; daisangen (open)
	(east east tsumo S6 S7 S8 E E E S S S W W N N W)					; shousuushii
	(east east tsumo S6 S7 S8 (open E E E) S S S W W N N W)				; shousuushii (open)
	(east east tsumo P3 P3 E E E S S S W W W N N N)						; daisuushii
	(east east tsumo P3 P3 (open E E E) S S S W W W N N N)				; daisuushii
	(east east tsumo E E S S S W W B B B F F F E)						; tsuuiisou
	(east east tsumo E E (open S S S) W W B B B F F F E)				; tsuuiisou (open)
	(east east tsumo M1 M1 P1 P1 P1 P9 P9 P9 S1 S1 S9 S9 S9 M1)			; chinroutou
	(east east tsumo M1 M1 (open P1 P1 P1) P9 P9 P9 S1 S1 S9 S9 S9 M1)	; chinroutou (open)
	(east east tsumo S2 S2 S3 S3 S4 S4 S6 S6 S8 S8 S8 F F S6)			; ryuuiisou
	(east east tsumo S2 S2 S3 S3 S4 S4 S6 S6 (open S8 S8 S8) F F S6)	; ryuuiisou (open)
	(east east tsumo P1 P1 P1 P2 P3 P4 P5 P6 P7 P8 P9 P9 P9 P4)			; chuuren poutou
	(east east tsumo (closed P1 P1 P1 P1) (closed P2 P2 P2 P2)			; suu kantsu
		  (closed S1 S1 S1 S1) (closed S2 S2 S2 S2) M5 M5)
	(east east tsumo (open P1 P1 P1 P1) (open P2 P2 P2 P2)				; suu kantsu (open)
		  (open S1 S1 S1 S1) (closed S2 S2 S2 S2) M5 M5)
	))

;(list-yakus)

(defun format-scoring (scoring)
  (let* ((han (scoring-han scoring))
		 (fu (scoring-fu scoring))
		 (limit (limit han fu))
		 (score (if limit limit (format nil "~d han ~d fu" han fu))))
	(if (equal han 0)
	  "Nothing."
	  (format nil "~:(~a~): ~{~(~a~) ~}" score (scoring-yakus scoring)))))

(defun score (hand)
  (let ((scorings (eval-all-orderings hand)))
	(reduce #'higher-scoring scorings)))

(defun main ()
  (handler-case
	(loop for line = (progn (format t "hand> " ) read-line)
		  for raw-hand = (with-input-from-string (in line) (read in))
		  for hand = (parse-hand raw-hand)
		  do (format t "~a~%" (format-scoring (score hand))))
	(condition () (format t "Error."))))

;(loop for raw-hand in (subseq *test-hands* 0)
;	  for hand = (parse-hand raw-hand)
;	  ;do (format t "~a~%" raw-hand)
;	  do (format t "~a~%" (format-scoring (score hand)))
;do (format t "~a~%" (sub-paths hand))
;do (format t "~a~%" hand)
;do (format t "~{~:a ~}~%~%" (eval-all-orderings hand))
;)

;(print (eval-all-orderings *hand*))


;(print *hand*)

;(sub-paths *hand*)

;(print (eval-all-orderings *hand*))

;(mapcar (lambda-of-suit :character) (parse-hand "P1 S2 M5"))

;(format nil "~a" (parse-tile "P1"))

