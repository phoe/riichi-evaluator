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
        #:riichi-evaluator.constants
        #:riichi-evaluator.tiles
        #:riichi-evaluator.set
        #:riichi-evaluator.hand)
  (:shadowing-import-from #:riichi-evaluator.set
                          #:set)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base))
  (:export))

(in-package #:riichi-evaluator.yaku)

;;; TODO: maybe move some of this to scoring.lisp

(defun count-fu (hand ordering)
  (if (chiitoi-p ordering)
      '((:chiitoitsu 25))
      (count-standard-fu hand ordering)))

(defun chiitoi-p (ordering)
  (and (typep (first ordering) 'toitsu)
       (every (a:rcurry #'typep 'antoi) (second ordering))))

(defun pinfu-p (ordering winning-tile)
  (destructuring-bind (winning-set other-sets) ordering
    (and (typep winning-set 'shuntsu)
         (every (a:rcurry #'typep '(or antoi anjun)) other-sets)
         (let* ((other-tiles (remove winning-tile (tiles winning-set)
                                     :test #'tile=))
                (other-ranks (mapcar #'rank other-tiles))
                (winning-rank (rank winning-tile)))
           (and
            ;; Not edge wait.
            (not (or (and (= 3 winning-rank)
                          (null (set-difference other-ranks '(1 2))))
                     (and (= 7 winning-rank)
                          (null (set-difference other-ranks '(8 9))))))
            ;; Not middle wait.
            (not (null (set-difference other-ranks
                                       (list (1+ winning-rank)
                                             (1- winning-rank))))))))))

(defun count-standard-fu (hand ordering)
  (let ((fu '())
        (all-sets (cons (first ordering) (second ordering))))
    (flet ((collect (x) (push x fu)))
      (collect '(20 :fuutei))
      (when (typep hand 'closed-ron-hand)
        (collect '(10 :menzen-kafu)))
      (dolist (set all-sets)
        (typecase set
          (minkou (collect (if (simple-p (same-tile-set-tile set))
                               `(2 :chunchanhai-minkou ,set)
                               `(4 :yaochuuhai-minkou ,set))))
          (ankou (collect (if (simple-p (same-tile-set-tile set))
                              `(4 :chunchanhai-minkou ,set)
                              `(8 :yaochuuhai-minkou ,set))))
          (minkan (collect (if (simple-p (same-tile-set-tile set))
                               `(8 :chunchanhai-minkan ,set)
                               `(16 :yaochuuhai-minkan ,set))))
          (ankan (collect (if (simple-p (same-tile-set-tile set))
                              `(16 :chunchanhai-ankan ,set)
                              `(32 :yaochuuhai-ankan ,set))))))
      (let* ((toitsu (find-if (a:rcurry #'typep 'toitsu) all-sets))
             (tile (same-tile-set-tile toitsu)))
        (when (dragon-p tile)
          (collect '(2 :sangenpai-toitsu)))
        (when (eq (kind tile) (prevailing-wind hand))
          (collect '(2 :bakaze-toitsu)))
        (when (eq (kind tile) (seat-wind hand))
          (collect '(2 :jikaze-toitsu))))
      ;; TODO: add fu for waits here.
      (let ((pinfu-p (pinfu-p ordering (winning-tile hand))))
        (when (and (not pinfu-p) (typep hand 'tsumo-hand))
          (collect '(2 :tsumo)))
        (when (and pinfu-p (typep hand 'open-hand))
          (collect '(2 :open-pinfu))))
      fu)))
