;;;; test/tiles.lisp
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

(in-package #:riichi-evaluator.test)

;;; Suited tiles

(defparameter *allowed-ranks* (alexandria:iota 9 :start 1))
(defparameter *allowed-suits* '(:number :circle :bamboo))
(defparameter *allowed-winds* '(:east :south :west :north))
(defparameter *allowed-dragons* '(:haku :hatsu :chun))
(defparameter *allowed-honors* (append *allowed-winds* *allowed-dragons*))

(defmacro with-all-suited-tiles ((rank suit tile) &body body)
  `(dolist (,suit *allowed-suits*)
     (declare (ignorable ,suit))
     (dolist (,rank *allowed-ranks*)
       (declare (ignorable ,rank))
       (let ((,tile (make-instance 'rt:suited-tile :rank ,rank :suit ,suit)))
         ,@body))))

(define-test suited-tiles-ok
  (with-all-suited-tiles (rank suit tile)
    (of-type rt:suited-tile tile)
    (true (rt:tile-p tile))
    (true (rt:suited-p tile))
    (cond ((member rank '(1 9))
           (true (rt:terminal-p tile))
           (false (rt:simple-p tile)))
          (t
           (false (rt:terminal-p tile))
           (true (rt:simple-p tile))))
    (false (rt:honor-p tile))
    (false (rt:wind-p tile))
    (false (rt:dragon-p tile))
    (dolist (suit-2 *allowed-suits*)
      (if (eq suit suit-2)
          (true (rt:of-suit suit-2 tile))
          (false (rt:of-suit suit-2 tile))))
    (dolist (rank-2 *allowed-ranks*)
      (if (eq rank rank-2)
          (true (rt:of-rank rank-2 tile))
          (false (rt:of-rank rank-2 tile))))
    (dolist (wind *allowed-winds*)
      (false (rt:of-wind wind tile)))
    (dolist (dragon *allowed-dragons*)
      (false (rt:of-dragon dragon tile)))
    (is = rank (rt:rank tile))
    (is eq suit (rt:suit tile))))

(define-test suited-tiles-tile=-tile<-tile-consec-p
  (with-all-suited-tiles (rank-1 suit-1 tile-1)
    (with-all-suited-tiles (rank-2 suit-2 tile-2)
      (if (and (eq suit-1 suit-2)
               (= rank-1 rank-2))
          (true (rt:tile= tile-1 tile-2))
          (false (rt:tile= tile-1 tile-2)))
      (let ((suit-1-position (position suit-1 *allowed-suits*))
            (suit-2-position (position suit-2 *allowed-suits*)))
        (if (or (< suit-1-position suit-2-position)
                (and (= suit-1-position suit-2-position)
                     (< rank-1 rank-2)))
            (true (rt:tile< tile-1 tile-2))
            (false (rt:tile< tile-1 tile-2))))
      (if (and (eq suit-1 suit-2)
               (= (mod (1+ rank-1) 9) (mod rank-2 9)))
          (true (rt:tile-consec-p tile-1 tile-2 :wrap-around t))
          (false (rt:tile-consec-p tile-1 tile-2 :wrap-around t)))
      (when (and (eq suit-1 suit-2)
                 (= 9 rank-1) (= 1 rank-2))
        (true (rt:tile-consec-p tile-1 tile-2 :wrap-around t))
        (false (rt:tile-consec-p tile-1 tile-2 :wrap-around nil))))))

;;; Honor tiles

(defmacro with-all-honor-tiles ((kind tile) &body body)
  `(dolist (,kind *allowed-honors*)
     (declare (ignorable ,kind))
     (let ((,tile (make-instance 'rt:honor-tile :kind ,kind)))
       ,@body)))

(define-test honor-tiles-ok
  (with-all-honor-tiles (kind tile)
    (of-type rt:honor-tile tile)
    (true (rt:tile-p tile))
    (false (rt:suited-p tile))
    (false (rt:terminal-p tile))
    (false (rt:simple-p tile))
    (true (rt:honor-p tile))
    (when (member kind *allowed-dragons*)
      (true (rt:dragon-p tile))
      (false (rt:wind-p tile)))
    (when (member kind *allowed-winds*)
      (false (rt:dragon-p tile))
      (true (rt:wind-p tile)))
    (dolist (suit *allowed-suits*)
      (false (rt:of-suit suit tile)))
    (dolist (rank *allowed-ranks*)
      (false (rt:of-rank rank tile)))
    (dolist (wind *allowed-winds*)
      (if (eq (rt:kind tile) wind)
          (true (rt:of-wind wind tile))
          (false (rt:of-wind wind tile))))
    (dolist (dragon *allowed-dragons*)
      (if (eq (rt:kind tile) dragon)
          (true (rt:of-dragon dragon tile))
          (false (rt:of-dragon dragon tile))))
    (is eq kind (rt:kind tile))))

(define-test honor-tiles-tile=-tile<-tile-consec-p
  (with-all-honor-tiles (kind-1 tile-1)
    (with-all-honor-tiles (kind-2 tile-2)
      (if (eq kind-1 kind-2)
          (true (rt:tile= tile-1 tile-2))
          (false (rt:tile= tile-1 tile-2)))
      (let ((kind-1-position (position kind-1 *allowed-honors*))
            (kind-2-position (position kind-2 *allowed-honors*)))
        (if (< kind-1-position kind-2-position)
            (true (rt:tile< tile-1 tile-2))
            (false (rt:tile< tile-1 tile-2))))
      (if (or (and (rt:dragon-p tile-1)
                   (rt:dragon-p tile-2)
                   (= (mod (1+ (position kind-1 *allowed-dragons*)) 3)
                      (mod (position kind-2 *allowed-dragons*) 3)))
              (and (rt:wind-p tile-1)
                   (rt:wind-p tile-2)
                   (= (mod (1+ (position kind-1 *allowed-winds*)) 4)
                      (mod (position kind-2 *allowed-winds*) 4))))
          (true (rt:tile-consec-p tile-1 tile-2 :wrap-around t))
          (false (rt:tile-consec-p tile-1 tile-2 :wrap-around t)))
      (false (rt:tile-consec-p tile-1 tile-2 :wrap-around nil)))))

;;; Mixed tiles

(define-test mixed-tiles-tile=-tile<-tile-consec-p
  (with-all-honor-tiles (kind honor-tile)
    (with-all-suited-tiles (rank suit suited-tile)
      (false (rt:tile= honor-tile suited-tile))
      (true (rt:tile< suited-tile honor-tile))
      (false (rt:tile< honor-tile suited-tile))
      (false (rt:tile-consec-p honor-tile suited-tile :wrap-around nil))
      (false (rt:tile-consec-p honor-tile suited-tile :wrap-around t))
      (false (rt:tile-consec-p suited-tile honor-tile :wrap-around nil))
      (false (rt:tile-consec-p suited-tile honor-tile :wrap-around t)))))

;;; Lisp reader

(defparameter *tile-reader-data*
  "([1m] [2m] [3m] [4m] [5m] [6m] [7m] [8m] [9m]
    [1p] [2p] [3p] [4p] [5p] [6p] [7p] [8p] [9p]
    [1s] [2s] [3s] [4s] [5s] [6s] [7s] [8s] [9s]
    [E]  [S]  [W]  [N]  [Hk] [Ht] [Ch])")

(define-test tile-reader
  (let* ((tiles-1 (uiop:while-collecting (collect)
                    (with-all-suited-tiles (rank suit tile) (collect tile))
                    (with-all-honor-tiles (kind tile) (collect tile))))
         (*readtable* (nr:find-readtable :riichi-evaluator))
         (tiles-2 (read-from-string *tile-reader-data*)))
    (is = 34 (length tiles-2))
    (loop for tile-1 in tiles-1
          for tile-2 in tiles-2
          do (is rt:tile= tile-1 tile-2))
    (let* ((string (with-output-to-string (stream) (print tiles-1 stream)))
           (tiles-3 (read-from-string string)))
      (is = 34 (length tiles-3))
      (loop for tile-1 in tiles-1
            for tile-3 in tiles-3
            do (is rt:tile= tile-1 tile-3)))))

;;; Tile list printer


