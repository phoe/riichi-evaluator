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
                    (#:p #:protest/base)
                    (#:nr #:named-readtables))
  (:export))

(in-package #:riichi-evaluator.yaku)

(nr:in-readtable :riichi-evaluator)

;;; TODO: maybe move some of this to scoring.lisp

(defun chiitoi-p (ordering)
  (and (typep (first ordering) 'toitsu)
       (every (a:rcurry #'typep 'antoi) (second ordering))))

(defun set-wait (set tile)
  (let* ((other-tiles (remove tile (tiles set) :test #'tile=))
         (other-ranks (mapcar #'rank other-tiles))
         (winning-rank (rank tile)))
    (flet ((other-ranks-are (&rest ranks)
             (null (set-difference other-ranks ranks))))
      (cond ((and (typep set 'koutsu) (tile= (same-tile-set-tile set) tile))
             :shanpon)
            ((and (typep set 'toitsu) (tile= (same-tile-set-tile set) tile))
             :tanki)
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
         (every (a:rcurry #'typep '(or antoi anjun)) other-sets)
         (eq :ryanmen (set-wait winning-set winning-tile)))))

(defun count-fu (hand ordering)
  (if (chiitoi-p ordering)
      '((:chiitoitsu 25))
      (destructuring-bind (winning-set other-sets) ordering
        (let* ((fu '())
               (winning-tile (winning-tile hand))
               (all-sets (cons winning-set other-sets)))
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
                                    `(4 :chunchanhai-ankou ,set)
                                    `(8 :yaochuuhai-ankou ,set))))
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
              (when (and (wind-p tile) (eq (kind tile) (prevailing-wind hand)))
                (collect '(2 :bakaze-toitsu)))
              (when (and (wind-p tile) (eq (kind tile) (seat-wind hand)))
                (collect '(2 :jikaze-toitsu))))
            (let ((wait (set-wait winning-set winning-tile)))
              (case wait
                (:kanchan (collect '(2 :kanchan)))
                (:penchan (collect '(2 :penchan)))
                (:tanki  (collect '(2 :tanki)))))
            (let ((pinfu-p (pinfu-p ordering winning-tile)))
              (when (and (not pinfu-p) (typep hand 'tsumo-hand))
                (collect '(2 :tsumo)))
              (when (and pinfu-p (typep hand 'open-hand))
                (collect '(2 :open-pinfu))))
            fu)))))
