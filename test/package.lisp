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
       (let ((,set (make-instance ,class :tile ,tile ,@args)))
         ,@body))))

(defmacro do-all-antoi ((set) &body body)
  `(do-all-same-tile-set (,set 'rs:antoi) ,@body))

(defmacro do-all-mintoi ((set) &body body)
  (a:with-gensyms (player)
    `(do-all-other-players (,player)
       (do-all-same-tile-set (,set 'rs:mintoi :taken-from ,player)
         ,@body))))

(defmacro do-all-ankou ((set) &body body)
  `(do-all-same-tile-set (,set 'rs:ankou) ,@body))

(defmacro do-all-minkou ((set) &body body)
  (a:with-gensyms (player)
    `(do-all-other-players (,player)
       (do-all-same-tile-set (,set 'rs:minkou :taken-from ,player)
         ,@body))))

(defmacro do-all-ankan ((set) &body body)
  `(do-all-same-tile-set (,set 'rs:ankan) ,@body))

(defmacro do-all-daiminkan ((set) &body body)
  (a:with-gensyms (player)
    `(do-all-other-players (,player)
       (do-all-same-tile-set (,set 'rs:daiminkan :taken-from ,player)
         ,@body))))

(defmacro do-all-shouminkan ((set) &body body)
  (a:with-gensyms (player)
    `(do-all-other-players (,player)
       (do-all-same-tile-set (,set 'rs:shouminkan :taken-from ,player)
         ,@body))))

(defmacro do-all-anjun ((set) &body body)
  (a:with-gensyms (tile)
    `(do-all-shuntsu-lower-tiles (,tile)
       (let ((,set (make-instance 'rs:anjun :lowest-tile ,tile)))
         ,@body))))

(defmacro do-all-minjun ((set) &body body)
  (a:with-gensyms (tile open-tile player)
    `(do-all-shuntsu-lower-tiles (,tile)
       (do-all-shuntsu-tiles (,tile ,open-tile)
         (do-all-other-players (,player)
           (let ((,set (make-instance 'rs:minjun :lowest-tile ,tile
                                                 :open-tile ,open-tile
                                                 :taken-from ,player)))
             ,@body))))))

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
                   ;; TODO: maybe add kokushi-musou.
                   ;; TODO: MAYBE add puutaa. Generating all valid puutaa hands
                   ;;       will be a pain unless we figure out a decent
                   ;;       algorithm for that.
                   )))))
