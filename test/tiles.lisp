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

(nr:in-readtable :riichi-evaluator)

;;; Protocol classes

(define-test tile-protocol
  (fail (make-instance 'rt:tile) 'p:protocol-error))

;;; Suited tiles

(define-test suited-tiles-ok
  (do-all-suited-tiles (rank suit tile)
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
  (do-all-suited-tiles (rank-1 suit-1 tile-1)
    (do-all-suited-tiles (rank-2 suit-2 tile-2)
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

(define-test suited-tile-negative
  (flet ((try (rank suit)
           (fail (make-instance 'rt:suited-tile :rank rank :suit suit)
               'rt:invalid-tile-datum)))
    (try 0 :bamboo)
    (try 10 :bamboo)
    (try :bamboo :bamboo)
    (try "bamboo" :bamboo)
    (try 1 :keyword)
    (try 1 "keyword")
    (try 1 0)))

;;; Honor tiles

(define-test honor-tiles-ok
  (do-all-honor-tiles (kind tile)
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
  (do-all-honor-tiles (kind-1 tile-1)
    (do-all-honor-tiles (kind-2 tile-2)
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

(define-test honor-tile-negative
  (flet ((try (kind)
           (fail (make-instance 'rt:honor-tile :kind kind)
               'rt:invalid-tile-datum)))
    (try 0)
    (try "east")
    (try "EAST")
    (try '(1 2 3))
    (try :keyword)))

;;; Mixed tiles

(define-test mixed-tiles-tile=-tile<-tile-consec-p
  (do-all-honor-tiles (kind honor-tile)
    (do-all-suited-tiles (rank suit suited-tile)
      (false (rt:tile= honor-tile suited-tile))
      (true (rt:tile< suited-tile honor-tile))
      (false (rt:tile< honor-tile suited-tile))
      (false (rt:tile-consec-p honor-tile suited-tile :wrap-around nil))
      (false (rt:tile-consec-p honor-tile suited-tile :wrap-around t))
      (false (rt:tile-consec-p suited-tile honor-tile :wrap-around nil))
      (false (rt:tile-consec-p suited-tile honor-tile :wrap-around t)))))

;;; Tile and tile list reader and printer

(defun tile-read-print-test (read-fn print-fn re-read-fn)
  (let* ((*readtable* (nr:find-readtable :riichi-evaluator))
         (tiles-1 (uiop:while-collecting (collect)
                    (do-all-suited-tiles (rank suit tile) (collect tile))
                    (do-all-honor-tiles (kind tile)
                      (collect tile) (collect tile))))
         (tiles-2 (funcall read-fn)))
    (is = 41 (length tiles-2))
    (loop for tile-1 in tiles-1
          for tile-2 in tiles-2
          do (is rt:tile= tile-1 tile-2))
    (let* ((string (funcall print-fn tiles-1))
           (tiles-3 (funcall re-read-fn string)))
      (is = 41 (length tiles-3))
      (loop for tile-1 in tiles-1
            for tile-3 in tiles-3
            do (is rt:tile= tile-1 tile-3)))))

(defparameter *tile-reader-data*
  "([1m] [2m] [3m] [4m] [5m] [6m] [7m] [8m] [9m]
    [1p] [2p] [3p] [4p] [5p] [6p] [7p] [8p] [9p]
    [1s] [2s] [3s] [4s] [5s] [6s] [7s] [8s] [9s]
    [EW] [1z] [SW] [2z] [WW] [3z] [NW] [4z]
    [WD] [5z] [GD] [6z] [RD] [7z])")

(define-test tile-reader
  (tile-read-print-test
   (lambda () (read-from-string *tile-reader-data*))
   (lambda (tiles) (prin1-to-string tiles))
   (lambda (string) (read-from-string string))))

(defparameter *tile-list-reader-data*
  "123456789m123456789p123456789s1234567z1234567z")

(define-test tile-list-reader
  (tile-read-print-test
   (lambda () (rt:read-tile-list-from-string *tile-list-reader-data*))
   (lambda (tiles) (rt:print-tile-list tiles nil))
   (lambda (string) (rt:read-tile-list-from-string string))))

(defparameter *make-tile-data*
  '("1m" "2m" "3m" "4m" "5m" "6m" "7m" "8m" "9m"
    "1p" "2p" "3p" "4p" "5p" "6p" "7p" "8p" "9p"
    "1s" "2s" "3s" "4s" "5s" "6s" "7s" "8s" "9s"
    "1z" "1z" "2z" "2z" "3z" "3z" "4z" "4z"
    "5z" "5z" "6z" "6z" "7z" "7z"))

(define-test make-tile
  (tile-read-print-test
   (lambda () (mapcar #'rt:make-tile *make-tile-data*))
   (lambda (tiles) (rt:print-tile-list tiles nil))
   (lambda (string) (rt:read-tile-list-from-string string))))
