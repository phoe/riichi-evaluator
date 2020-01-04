;;;; test/package.lisp
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

(defpackage #:riichi-evaluator.test
  (:local-nicknames (#:rc #:riichi-evaluator.constants)
                    (#:rt #:riichi-evaluator.tiles)
                    (#:rs #:riichi-evaluator.set)
                    (#:rh #:riichi-evaluator.hand)
                    (#:nr #:named-readtables)
                    (#:p #:protest/base)
                    (#:a #:alexandria))
  (:use #:cl
        #:parachute))

(in-package #:riichi-evaluator.test)

;;; Data and helper macros

(defparameter *allowed-ranks* (alexandria:iota 9 :start 1))
(defparameter *allowed-shuntsu-ranks* (alexandria:iota 7 :start 1))
(defparameter *allowed-suits* '(:number :circle :bamboo))
(defparameter *allowed-winds* '(:east :south :west :north))
(defparameter *allowed-dragons* '(:haku :hatsu :chun))
(defparameter *allowed-honors* (append *allowed-winds* *allowed-dragons*))
(defparameter *allowed-players* '(:shimocha :toimen :kamicha))

(defparameter *suited-tiles*
  (uiop:while-collecting (collect)
    (dolist (suit *allowed-suits*)
      (dolist (rank *allowed-ranks*)
        (collect (make-instance 'rt:suited-tile :rank rank :suit suit))))))

(defparameter *honor-tiles*
  (uiop:while-collecting (collect)
    (dolist (kind *allowed-honors*)
      (collect (make-instance 'rt:honor-tile :kind kind)))))

(defparameter *all-tiles*
  (append *suited-tiles* *honor-tiles*))

(defmacro do-all-suited-tiles ((rank suit tile) &body body)
  `(dolist (,suit *allowed-suits*)
     (declare (ignorable ,suit))
     (dolist (,rank *allowed-ranks*)
       (declare (ignorable ,rank))
       (let ((,tile (make-instance 'rt:suited-tile :rank ,rank :suit ,suit)))
         ,@body))))

(defmacro do-all-shuntsu-lower-tiles ((tile) &body body)
  (a:with-gensyms (rank suit)
    `(dolist (,suit *allowed-suits*)
       (declare (ignorable ,suit))
       (dolist (,rank *allowed-shuntsu-ranks*)
         (declare (ignorable ,rank))
         (let ((,tile (make-instance 'rt:suited-tile :rank ,rank :suit ,suit)))
           ,@body)))))

(defmacro do-all-honor-tiles ((kind tile) &body body)
  `(dolist (,kind *allowed-honors*)
     (declare (ignorable ,kind))
     (let ((,tile (make-instance 'rt:honor-tile :kind ,kind)))
       ,@body)))

(defmacro do-all-tiles ((tile) &body body)
  `(dolist (,tile *all-tiles*) ,@body))

(defmacro do-all-other-players ((player) &body body)
  `(dolist (,player *allowed-players*) ,@body))

(defmacro do-all-shuntsu-tiles ((orig-tile tile) &body body)
  (a:with-gensyms (make suit rank tile-2 tile-3)
    `(flet ((,make (,rank ,suit)
              (make-instance 'rt:suited-tile :suit ,suit :rank ,rank)))
       (let* ((,suit (rt:suit ,orig-tile))
              (,rank (rt:rank ,orig-tile))
              (,tile-2 (,make (+ ,rank 1) ,suit))
              (,tile-3 (,make (+ ,rank 2) ,suit)))
         (dolist (,tile (list ,orig-tile ,tile-2 ,tile-3))
           ,@body)))))

(defmacro do-all-same-tile-set ((set class &rest args) &body body)
  (a:with-gensyms (tile)
    `(do-all-tiles (,tile)
       (let ((,set (,class ,tile ,@args)))
         ,@body))))

(defmacro do-all-antoi ((set) &body body)
  `(do-all-same-tile-set (,set rs:antoi) ,@body))

(defmacro do-all-mintoi ((set) &body body)
  (a:with-gensyms (player)
    `(do-all-other-players (,player)
       (do-all-same-tile-set (,set rs:mintoi ,player)
         ,@body))))

(defmacro do-all-ankou ((set) &body body)
  `(do-all-same-tile-set (,set rs:ankou) ,@body))

(defmacro do-all-minkou ((set) &body body)
  (a:with-gensyms (player)
    `(do-all-other-players (,player)
       (do-all-same-tile-set (,set rs:minkou ,player)
         ,@body))))

(defmacro do-all-ankan ((set) &body body)
  `(do-all-same-tile-set (,set rs:ankan) ,@body))

(defmacro do-all-daiminkan ((set) &body body)
  (a:with-gensyms (player)
    `(do-all-other-players (,player)
       (do-all-same-tile-set (,set rs:daiminkan ,player)
         ,@body))))

(defmacro do-all-shouminkan ((set) &body body)
  (a:with-gensyms (player)
    `(do-all-other-players (,player)
       (do-all-same-tile-set (,set rs:shouminkan ,player)
         ,@body))))

(defmacro do-all-anjun ((set) &body body)
  (a:with-gensyms (tile)
    `(do-all-shuntsu-lower-tiles (,tile)
       (let ((,set (rs:anjun ,tile)))
         ,@body))))

(defmacro do-all-minjun ((set) &body body)
  (a:with-gensyms (tile open-tile player)
    `(do-all-shuntsu-lower-tiles (,tile)
       (do-all-shuntsu-tiles (,tile ,open-tile)
         (do-all-other-players (,player)
           (let ((,set (rs:minjun ,tile ,open-tile ,player)))
             ,@body))))))

(defmacro do-all-closed-kokushi-musou ((set) &body body)
  (a:with-gensyms (tile)
    `(dolist (,tile rs:*kokushi-musou-tiles*)
       (let ((,set (rs:closed-kokushi-musou ,tile)))
         ,@body))))

(defmacro do-all-open-kokushi-musou ((set) &body body)
  (a:with-gensyms (tile open-tile player)
    `(dolist (,tile rs:*kokushi-musou-tiles*)
       (dolist (,open-tile rs:*kokushi-musou-tiles*)
         (do-all-other-players (,player)
           (let ((,set (rs:open-kokushi-musou ,tile ,open-tile ,player)))
             ,@body))))))

(defmacro do-all-kokushi-musou ((set) &body body)
  (a:with-gensyms (thunk)
    `(flet ((,thunk (,set) ,@body))
       (do-all-closed-kokushi-musou (,set) (funcall #',thunk ,set))
       (do-all-open-kokushi-musou (,set) (funcall #',thunk ,set)))))

(defmacro do-all-valid-sets ((set) &body body)
  (a:with-gensyms (thunk)
    `(flet ((,thunk (,set) ,@body))
       ,@(mapcar (lambda (x) `(,x (set) (,thunk set)))
                 '(do-all-antoi
                   do-all-mintoi
                   do-all-ankou
                   do-all-minkou
                   do-all-ankan
                   do-all-daiminkan
                   do-all-shouminkan
                   do-all-anjun
                   do-all-minjun
                   ;; NOTE: kokushi musou and puutaa are not listed here. The
                   ;; number of all possible sets listed above is 720, and that
                   ;; number is squared during the exhaustive SET= test.
                   ;; If we wanted to iterate over all sets we can compute, we
                   ;; would need to consider 507 more sets for possible kokushi
                   ;; musou, 81030 possible sets for shiisuu puutaa, and 3771950
                   ;; possible sets for shiisan puutaa, and THEN square that
                   ;; number for the SET= test. The current computers cannot
                   ;; easily handle such a combinatorial explosion.
                   )))))

;;; Puutaa tiles generator

;;; NOTE:
;;;
;;; In puutaa hands, at most three tiles can come from a single numeric suit,
;;; which means that at most 9 tiles can be from all three numeric suits, which
;;; in turn means that at least 5 tiles must be honors. At the same, at most 7
;;; tiles can be honors, since that is the maximum number of different honors we
;;; may have.
;;;
;;; A shiisuu puutaa hand contains 14 single tiles out of 34.
;;; This means that, if we want to draw 14 tiles in total, then, for the numbers
;;; of (NUMBERS CIRCLES BAMBOOS) tiles:
;;; * For 5 honors, we must draw (3 3 3) tiles;
;;; * For 6 honors, we must draw (3 3 2), (3 2 3), or (3 3 2) tiles;
;;; * For 7 honors, we must draw (3 3 1), (3 1 3), (1 3 3), (3 2 2), (2 3 2), or
;;;   (2 2 3) tiles.
;;;
;;; A shiisan puutaaa hand contains 13 single tiles out of 34 (with one
;;; duplicate).
;;; This means that, if we want to draw 13 tiles in total, then, for the numbers
;;; of (NUMBERS CIRCLES BAMBOOS) tiles:
;;; * For 4 honors, we must draw (3 3 3) tiles;
;;; * For 5 honors, we must draw (3 3 2), (3 2 3), or (3 3 2) tiles;
;;; * For 6 honors, we must draw (3 3 1), (3 1 3), (1 3 3), (3 2 2), (2 3 2), or
;;;   (2 2 3) tiles.
;;; * For 7 honors, we must draw (3 3 0), (3 0 3), (0 3 3), (3 2 1), (3 1 2),
;;;   (2 1 3), (2 3 1), (1 2 3), or (1 3 2) tiles.

(defun valid-puutaa-ranks (number-of-tiles)
  (case number-of-tiles
    (3 '((1 4 7) (1 4 8) (1 4 9) (1 5 8) (1 5 9) (1 6 9)
         (2 5 8) (2 5 9) (2 6 9)
         (3 6 9)))
    (2 '((1 4) (1 5) (1 6) (1 7) (1 8) (1 9)
         (2 5) (2 6) (2 7) (2 8) (2 9)
         (3 6) (3 7) (3 8) (3 9)
         (4 7) (4 8) (4 9)
         (5 8) (5 9)
         (6 9)))
    (1 '((1) (2) (3) (4) (5) (6) (7) (8) (9)))))

(defun puutaa-suites-draw-ranks (number-of-tiles)
  (ecase number-of-tiles
    (9 '((3 3 3)))
    (8 '((3 3 2) (3 2 3) (2 3 3)))
    (7 '((3 3 1) (3 1 3) (1 3 3)
         (3 2 2) (2 3 2) (2 2 3)))
    (6 '((3 3 0) (3 0 3) (0 3 3)
         (3 2 1) (3 1 2) (2 1 3)
         (2 3 1) (1 2 3) (1 3 2)))))

(defun puutaa-numbers-of-tiles (puutaa-kind)
  (ecase puutaa-kind
    (:shiisuu-puutaa '((5 9) (6 8) (7 7)))
    (:shiisan-puutaa '((4 9) (5 8) (6 7) (7 6)))))

(defmacro do-all-puutaa-tiles-in-suit
    ((tile-list suit number-of-tiles) &body body)
  (a:with-gensyms (ranks make rank)
    `(dolist (,ranks (valid-puutaa-ranks ,number-of-tiles))
       (flet ((,make (,rank)
                (make-instance 'rt:suited-tile :rank ,rank :suit ,suit)))
         (let ((,tile-list (mapcar #',make ,ranks)))
           ,@body)))))

(defmacro do-all-puutaa-tiles-in-suits
    ((tile-list number-of-suited-tiles) &body body)
  (a:with-gensyms (draws
                   numbers circles bamboos
                   number-tiles circle-tiles bamboo-tiles)
    `(dolist (,draws (puutaa-suites-draw-ranks ,number-of-suited-tiles))
       (destructuring-bind (,numbers ,circles, bamboos) ,draws
         (do-all-puutaa-tiles-in-suit (,number-tiles :number ,numbers)
           (do-all-puutaa-tiles-in-suit (,circle-tiles :circle ,circles)
             (do-all-puutaa-tiles-in-suit (,bamboo-tiles :bamboo ,bamboos)
               (let ((,tile-list (append ,number-tiles
                                         ,circle-tiles
                                         ,bamboo-tiles)))
                 ,@body))))))))

(defmacro do-all-honor-combinations ((honor-list length) &body body)
  (a:with-gensyms (honor-tiles)
    `(let ((,honor-tiles (mapcar (a:curry #'make-instance 'rt:honor-tile :kind)
                                 *allowed-honors*)))
       (a:map-combinations (lambda (,honor-list) ,@body) ,honor-tiles
                           :length ,length))))

(defmacro do-all-shiisuu-puutaa-tiles ((tile-list) &body body)
  (a:with-gensyms (tiles-length
                   honors-length suited-length
                   honor-list suited-list)
    `(dolist (,tiles-length (puutaa-numbers-of-tiles :shiisuu-puutaa))
       (destructuring-bind (,honors-length ,suited-length) ,tiles-length
         (do-all-honor-combinations (,honor-list ,honors-length)
           (do-all-puutaa-tiles-in-suits (,suited-list ,suited-length)
             (let ((,tile-list (append ,honor-list ,suited-list)))
               ,@body)))))))

(defmacro do-all-shiisan-puutaa-tiles ((pair-tile single-tiles) &body body)
  (a:with-gensyms (tiles-length
                   honors-length suited-length
                   honor-list suited-list
                   tile-list-without-pair)
    `(dolist (,tiles-length (puutaa-numbers-of-tiles :shiisan-puutaa))
       (destructuring-bind (,honors-length ,suited-length) ,tiles-length
         (do-all-honor-combinations (,honor-list ,honors-length)
           (do-all-puutaa-tiles-in-suits (,suited-list ,suited-length)
             (let ((,tile-list-without-pair (append ,honor-list ,suited-list)))
               (dolist (,pair-tile ,tile-list-without-pair)
                 (let ((,single-tiles (remove ,pair-tile ,tile-list-without-pair
                                              :test #'rt:tile=)))
                   ,@body)))))))))

(defmacro do-all-shiisuu-puutaa-sets ((set) &body body)
  `(do-all-shiisuu-puutaa-tiles (tile-list)
     (let ((,set (rs:shiisuu-puutaa (copy-list tile-list))))
       ,@body)))

(defmacro do-all-shiisan-puutaa-sets ((set) &body body)
  (a:with-gensyms (pair-tile single-tiles)
    `(do-all-shiisan-puutaa-tiles (,pair-tile ,single-tiles)
       (let* ((,set (rs:shiisan-puutaa ,pair-tile ,single-tiles)))
         ,@body))))
