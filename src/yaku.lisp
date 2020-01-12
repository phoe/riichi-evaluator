;;;; src/yaku.lisp
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

(defpackage #:riichi-evaluator.yaku
  (:use #:cl
        #:destructuring-bind-star
        #:riichi-evaluator.constants
        #:riichi-evaluator.tiles
        #:riichi-evaluator.set
        #:riichi-evaluator.hand)
  (:shadowing-import-from #:riichi-evaluator.set
                          #:set)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:nr #:named-readtables))
  (:export
   #:chiitoitsu-p #:set-wait #:pinfu-p
   #:count-fu))

(in-package #:riichi-evaluator.yaku)

(nr:in-readtable :riichi-evaluator)

;;; Fu counter

(defun chiitoitsu-p (ordering)
  (and (typep (first ordering) 'toitsu)
       (every (a:rcurry #'typep 'antoi) (second ordering))))

(defun set-wait (set tile)
  (let* ((other-tiles (remove tile (tiles set) :test #'tile=))
         (other-ranks (mapcar #'rank other-tiles))
         (winning-rank (rank tile)))
    (flet ((other-ranks-are (&rest ranks)
             (null (set-difference other-ranks ranks))))
      (cond ((and (typep set 'toitsu) (tile= (same-tile-set-tile set) tile))
             :tanki)
            ((and (typep set 'koutsu) (tile= (same-tile-set-tile set) tile))
             :shanpon)
            ((and (typep set 'shuntsu)
                  (or (and (= 3 winning-rank) (other-ranks-are 1 2))
                      (and (= 7 winning-rank) (other-ranks-are 8 9))))
             :penchan)
            ((and (typep set 'shuntsu)
                  (other-ranks-are (1+ winning-rank) (1- winning-rank)))
             :kanchan)
            ((and (typep set 'shuntsu)
                  (or (other-ranks-are (+ winning-rank 2) (+ winning-rank 1))
                      (other-ranks-are (- winning-rank 2) (- winning-rank 1))))
             :ryanmen)))))

(defun pinfu-p (ordering winning-tile)
  (destructuring-bind (winning-set other-sets) ordering
    (and (typep winning-set 'shuntsu)
         (= 1 (count-if (a:rcurry #'typep 'antoi) other-sets))
         (= 3 (count-if (a:rcurry #'typep 'anjun) other-sets))
         (eq :ryanmen (set-wait winning-set winning-tile)))))

(defun count-fu (hand ordering)
  (if (chiitoitsu-p ordering)
      '((:chiitoitsu 25))
      (destructuring-bind (winning-set other-sets) ordering
        (let* ((fu '())
               (winning-tile (winning-tile hand))
               (all-sets (cons winning-set other-sets)))
          (flet ((collect (x) (push x fu)))
            ;; Fuutei and menzen-kafu.
            (collect '(20 :fuutei))
            (when (typep hand 'closed-ron-hand)
              (collect '(10 :menzen-kafu)))
            ;; Fu from sets.
            (dolist (set all-sets)
              (typecase set
                (minkou (collect (if (simple-p (same-tile-set-tile set))
                                     `(2 :chunchanhai-minkou ,set)
                                     `(4 :yaochuuhai-minkou ,set))))
                (ankou (collect (if (simple-p (same-tile-set-tile set))
                                    `(4 :chunchanhai-ankou ,set)
                                    `(8 :yaochuuhai-ankou ,set))))
                (minkan (collect (if (simple-p (same-tile-set-tile set))
                                     `(8 :chunchanhai-minkan ,set)
                                     `(16 :yaochuuhai-minkan ,set))))
                (ankan (collect (if (simple-p (same-tile-set-tile set))
                                    `(16 :chunchanhai-ankan ,set)
                                    `(32 :yaochuuhai-ankan ,set))))))
            ;; Fu from toitsu of dragon/active wind.
            (let* ((toitsu (find-if (a:rcurry #'typep 'toitsu) all-sets))
                   (tile (same-tile-set-tile toitsu)))
              (when (dragon-p tile)
                (collect '(2 :sangenpai-atama)))
              (when (and (wind-p tile) (eq (kind tile) (prevailing-wind hand)))
                (collect '(2 :bakaze-atama)))
              (when (and (wind-p tile) (eq (kind tile) (seat-wind hand)))
                (collect '(2 :jikaze-atama))))
            ;; Fu from difficult waits.
            (let ((wait (set-wait winning-set winning-tile)))
              (case wait
                (:kanchan (collect '(2 :kanchan)))
                (:penchan (collect '(2 :penchan)))
                (:tanki  (collect '(2 :tanki)))))
            ;; Fu from tsumo/open pinfu.
            (let ((pinfu-p (pinfu-p ordering winning-tile)))
              (when (and (not pinfu-p) (typep hand 'tsumo-hand))
                (collect '(2 :tsumo)))
              (when (and pinfu-p (typep hand 'open-hand))
                (collect '(2 :open-pinfu))))
            fu)))))

;;; Situations

(define-condition invalid-situation (invalid-hand simple-condition)
  ((%situation :reader invalid-situation-situation :initarg :situation))
  (:default-initargs
   :situation (a:required-argument :situation)
   :format-control "No reason given.")
  (:report
   (lambda (condition stream)
     (let ((situation (invalid-situation-situation condition)))
       (format stream "Invalid situation ~S for hand ~S:~%~A"
               (if (and (consp situation) (null (cdr situation)))
                   (car situation)
                   situation)
               (invalid-hand-hand condition)
               (apply #'format nil
                      (simple-condition-format-control condition)
                      (simple-condition-format-arguments condition)))))))

(defun invalid-situation (hand situation args format-control &rest format-args)
  (error 'invalid-situation :hand hand :situation (cons situation args)
                            :format-control format-control
                            :format-args format-args))

(define-condition invalid-dora-list-lengths (invalid-hand)
  ((%dora-list :reader dora-list :initarg :dora-list)
   (%ura-dora-list :reader ura-dora-list :initarg :ura-dora-list))
  (:default-initargs
   :dora-list (a:required-argument :dora-list)
   :ura-dora-list (a:required-argument :ura-dora-list))
  (:report
   (lambda (condition stream)
     (format stream "The dora list ~S and ura dora list ~S for hand ~S ~
                       are not of the same length."
             (dora-list condition)
             (ura-dora-list condition)
             (invalid-hand-hand condition)))))

(defun check-dora-ura-dora-list-length (hand)
  (let ((dora-list-length (length (dora-list hand)))
        (ura-dora-list-length (length (ura-dora-list hand))))
    (unless (= dora-list-length ura-dora-list-length)
      (error 'invalid-dora-list-lengths
             :hand hand
             :dora-list (dora-list hand)
             :ura-dora-list (ura-dora-list hand)))))

(defmacro define-situation
    (name (hand situation &optional (args (gensym "ARGS")))
     lambda-list &body body)
  (check-type name (or null keyword))
  `(defmethod validate-situation progn
       (,hand ,(if name `(,situation (eql ,name)) situation) &rest ,args)
     (handler-case (destructuring-bind* ,lambda-list ,args ,@body)
       (destructuring-error ()
         (invalid-situation ,hand ,situation ,args
                            "Situation ~A expected arguments in format ~
                               ~S, but got ~S instead."
                            (list ,situation ',lambda-list ',args))))))

(define-situation nil (hand situation args) ()
  (when (null (compute-applicable-methods
               #'validate-situation (list* hand situation)))
    (invalid-situation hand situation args "Unknown situation ~S." situation)))

;;; Yaku

(defgeneric compute-all-yaku (hand ordering)
  (:method-combination chained-append))

(defmacro define-yaku (name (hand &key
                                    (ordering (gensym "ORDERING"))
                                    (sets (gensym "SETS")))
                       &body body)
  (check-type name keyword)
  `(defmethod compute-all-yaku ,name (,hand ,ordering)
     (declare (ignorable ,hand ,ordering))
     (when (let ((,sets (cons (first ,ordering) (second ,ordering))))
             (declare (ignorable ,sets))
             ,@body)
       '(,name))))

(defgeneric compute-incompatible-yaku (yaku-list) ;; TODO
  (:method-combination chained-append))

(defun compute-yaku (hand ordering)
  (let* ((yaku-list (compute-all-yaku hand ordering))
         (incompatible-yaku (compute-incompatible-yaku yaku-list)))
    (set-difference yaku-list incompatible-yaku)))

;;; Riichi

(define-situation :riichi (hand situation) ()
  (check-dora-ura-dora-list-length hand)
  (when (typep hand 'open-hand)
    (invalid-situation hand situation '()
                       "Riichi cannot be declared on an open hand.")))

(define-yaku :riichi (hand)
  (member :riichi (situations hand) :key #'a:ensure-car))

;;; Double riichi

(define-situation :double-riichi (hand situation) ()
  (unless (member :riichi (situations hand))
    (invalid-situation hand situation '()
                       "Double riichi cannot occur without riichi.")))

(define-yaku :double-riichi (hand)
  (member :double-riichi (situations hand) :key #'a:ensure-car))

;;; Ippatsu

(define-situation :ippatsu (hand situation) ()
  (unless (member :riichi (situations hand))
    (invalid-situation hand situation '()
                       "Ippatsu cannot occur without riichi.")))

(define-yaku :ippatsu (hand)
  (member :ippatsu (situations hand) :key #'a:ensure-car))

;;; Menzenchin tsumohou

(define-yaku :menzenchin-tsumohou (hand)
  (typep hand 'closed-tsumo-hand))

;;; Tanyao

(define-yaku :tanyao (hand)
  (and (simple-p (winning-tile hand))
       (every #'simple-p (free-tiles hand))
       (every #'simple-p (a:mappend #'tiles (locked-sets hand)))))

;;; Pinfu

(define-yaku :pinfu (hand :ordering ordering)
  (pinfu-p ordering (winning-tile hand)))

;;; Iipeikou

(define-yaku :iipeikou (hand :sets sets)
  (and (typep hand 'closed-hand)
       (loop for set in sets
             when (and (typep set 'anjun) (= 2 (count set sets :test #'set=)))
               return t)))

;;; Ikkitsuukan

(define-yaku :ikkitsuukan (hand :sets sets)
  (flet ((make-pred (rank suit)
           (lambda (x) (and (typep x 'shuntsu)
                            (let ((tile (shuntsu-lowest-tile x)))
                              (eql suit (suit tile))
                              (= rank (rank tile)))))))
    (loop for suit in *suits*
            thereis (and (find-if (make-pred 1 suit) sets)
                         (find-if (make-pred 4 suit) sets)
                         (find-if (make-pred 7 suit) sets)))))

;;; Bakaze

(macrolet ((bakaze (wind)
             `(define-yaku ,(a:format-symbol :keyword "BAKAZE-~A" wind)
                  (hand :sets sets)
                (and (eq ,wind (prevailing-wind hand))
                     (find-if (lambda (x)
                                (and (typep x '(or kantsu koutsu))
                                     (tile= (same-tile-set-tile x)
                                            (make-instance 'honor-tile
                                                           :kind ,wind))))
                              sets))))
           (make () `(progn ,@(mapcar (lambda (x) `(bakaze ,x)) *winds*))))
  (make))

;;; Jikaze

(macrolet ((jikaze (wind)
             `(define-yaku ,(a:format-symbol :keyword "JIKAZE-~A" wind)
                  (hand :sets sets)
                (and (eq ,wind (seat-wind hand))
                     (find-if (lambda (x)
                                (and (typep x '(or kantsu koutsu))
                                     (tile= (same-tile-set-tile x)
                                            (make-instance 'honor-tile
                                                           :kind ,wind))))
                              sets))))
           (make () `(progn ,@(mapcar (lambda (x) `(jikaze ,x)) *winds*))))
  (make))

;;; Sangenpai

(macrolet ((yakuhai (dragon)
             `(define-yaku ,(a:format-symbol :keyword "YAKUHAI-~A" dragon)
                  (hand :sets sets)
                (find-if (lambda (x)
                           (and (typep x '(or kantsu koutsu))
                                (tile= (same-tile-set-tile x)
                                       (make-instance 'honor-tile
                                                      :kind ,dragon))))
                         sets)))
           (make () `(progn ,@(mapcar (lambda (x) `(yakuhai ,x))
                                      '(:haku :hatsu :chun)))))
  (make))

;;; Sanshoku doujun

(define-yaku :sanshoku-doujun (hand :sets sets)
  (dolist (set sets)
    (when (typep set 'shuntsu)
      (let* ((tile (shuntsu-lowest-tile set))
             (rank (rank tile))
             (suit (suit tile)))
        (flet ((doujun-set-p (set suit)
                 (and (and (typep set 'shuntsu)
                           (let ((tile (shuntsu-lowest-tile set)))
                             (eq rank (rank tile))
                             (eq suit (suit tile)))))))
          (destructuring-bind (other-rank-1 other-rank-2) (remove suit *suits*)
            (when (and (find-if (a:rcurry #'doujun-set-p other-rank-1) sets)
                       (find-if (a:rcurry #'doujun-set-p other-rank-2) sets))
              (return t))))))))

;;; Sanshoku doukou

(define-yaku :sanshoku-doukou (hand :sets sets)
  (flet ((doukou-set-p (set suit)
           (and (and (typep set '(or koutsu kantsu))
                     (eq suit (suit (same-tile-set-tile set)))))))
    (dolist (set sets)
      (when (and (typep set '(or koutsu kantsu))
                 (suited-p (same-tile-set-tile set)))
        (let ((suit (suit (same-tile-set-tile set))))
          (destructuring-bind (other-suit-1 other-suit-2) (remove suit *suits*)
            (when (and (find-if (a:rcurry #'doukou-set-p other-suit-1) sets)
                       (find-if (a:rcurry #'doukou-set-p other-suit-2) sets))
              (return t))))))))

;;; Toitoihou

(define-yaku :toitoihou (hand :sets sets)
  (and (= 1 (count-if (a:rcurry #'typep 'toitsu) sets))
       (= 4 (count-if (a:rcurry #'typep '(or koutsu kantsu)) sets))))

;;; Sanankou

(define-yaku :sanankou (hand :sets sets)
  (and (= 3 (count-if (a:rcurry #'typep '(or ankou ankan)) sets))))

;;; Sankantsu

(define-yaku :sankantsu (hand :sets sets)
  (and (= 3 (count-if (a:rcurry #'typep 'kantsu) sets))))

;;; TODO: Honchantaiyaochuu
;;; TODO: Junchantaiyaochuu

;;; Ryanpeikou

(define-yaku :ryanpeikou (hand :sets sets)
  (and (typep hand 'closed-hand)
       (dolist (set sets)
         (and (typep set 'anjun) (= 2 (count set sets :test #'set=))
              (let ((other-sets (remove set sets :test #'set= :count 2)))
                (dolist (other-set other-sets)
                  (when (and (typep other-set 'anjun)
                             (= 2 (count other-set other-sets :test #'set=)))
                    (return t))))
              (return t)))))

;;; Shousangen

(define-yaku :shousangen (hand :sets sets)
  (flet ((dragon-set-p (set)
           (and (typep set 'same-tile-set)
                (dragon-p (same-tile-set-tile set)))))
    (and (= 1 (count-if (a:conjoin #'dragon-set-p
                                   (a:rcurry #'typep 'toitsu))
                        sets))
         (= 2 (count-if (a:conjoin #'dragon-set-p
                                   (a:rcurry #'typep '(or koutsu kantsu)))
                        sets)))))

;;; Honroutou

(define-yaku :honroutou (hand)
  (flet ((honroutou-tile-p (tile)
           (or (terminal-p tile) (honor-p tile))))
    (and (honroutou-tile-p (winning-tile hand))
         (every #'honroutou-tile-p (free-tiles hand))
         (every #'honroutou-tile-p
                (a:mappend #'tiles (locked-sets hand))))))

;;; Honiisou

(define-yaku :honiisou (hand)
  (dolist (suit *suits*)
    (flet ((honiisou-tile-p (tile)
             (or (eq (suit tile) suit) (eq (suit tile) :honor))))
      (when (and (honiisou-tile-p (winning-tile hand))
                 (every #'honiisou-tile-p (free-tiles hand))
                 (every #'honiisou-tile-p
                        (a:mappend #'tiles (locked-sets hand))))
        (return t)))))

;;; Chiniisou

(define-yaku :chiniisou (hand)
  (dolist (suit *suits*)
    (flet ((chiniisou-tile-p (tile) (eq (suit tile) suit)))
      (when (and (chiniisou-tile-p (winning-tile hand))
                 (every #'chiniisou-tile-p (free-tiles hand))
                 (every #'chiniisou-tile-p
                        (a:mappend #'tiles (locked-sets hand))))
        (return t)))))

;;; Chiitoitsu

(define-yaku :chiitoitsu (hand :ordering ordering)
  (chiitoitsu-p ordering))

;;; TODO: Rinshan kaihou
;;; TODO: Haitei raoyue
;;; TODO: Houtei raoyui
;;; TODO: Chankan
;;; TODO: Kokushi musou
;;; TODO: Kokushi musou juusan menmachi
;;; TODO: Daisangen
;;; TODO: Suuankou
;;; TODO: Suuankou tanki
;;; TODO: Shoosuushii
;;; TODO: Daisuushii
;;; TODO: Tsuuiisou
;;; TODO: Ryuuiisou
;;; TODO: Chinroutou
;;; TODO: Chuuren poutou
;;; TODO: Junsei chuuren poutou
;;; TODO: Suukantsu
;;; TODO: Tenhou
;;; TODO: Chiihou

;;; TODO: Renhou
;;; TODO: Sanrenkou
;;; TODO: Iishoku sanjun
;;; TODO: Shiisan puutaa
;;; TODO: Shousharin
;;; TODO: Kinkei dokuritsu
;;; TODO: Nagashi mangan

;;; Open riichi

(define-situation :open-riichi (hand situation) ()
  (unless (member :riichi (situations hand))
    (invalid-situation hand situation '()
                       "Open riichi cannot occur without riichi.")))

(define-yaku :open-riichi (hand :ordering ordering)
  (member :open-riichi (situations hand) :key #'a:ensure-car))

;;; TODO: Otakaze sankou
;;; TODO: Uumensai
;;; TODO: Kinmonkyou
;;; TODO: Tsubame gaeshi
;;; TODO: Suurenkou
;;; TODO: Iishoku yonjun
;;; TODO: Shiisuu puutaa
;;; TODO: Daichiisei
;;; TODO: Daisharin
;;; TODO: Beni kujaku
;;; TODO: Paarenchan
;;; TODO: Go zoku kyouwa
;;; TODO: Daikinmonkyou
;;; TODO: Hyakuman goku
;;; TODO: Ao no doumon
;;; TODO: Kaachou fuugetsu
;;; TODO: Fuuka setsugetsu
