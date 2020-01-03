;;;; test/set.lisp
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

(define-test set-protocol
  (flet ((try (class &rest args)
           (fail (apply #'make-instance class args) 'p:protocol-error)))
    (try 'rs:set)
    (try 'rs:same-tile-set :tile (rt:make-tile "1p"))
    (try 'rs:closed-set)
    (try 'rs:open-set :taken-from :kamicha)
    (try 'rs:shuntsu :lowest-tile (rt:make-tile "1p"))
    (try 'rs:toitsu :tile (rt:make-tile "1p"))
    (try 'rs:koutsu :tile (rt:make-tile "1p"))
    (try 'rs:kantsu :tile (rt:make-tile "1p"))))

(define-test set-toitsu-positive
  (do-all-tiles (tile)
    (flet ((verify-tiles (set)
             (let ((tiles (rs:tiles set)))
               (is = 2 (length tiles))
               (true (every (a:curry #'rt:tile= tile) tiles)))))
      (let* ((antoi (make-instance 'rs:antoi :tile tile)))
        (verify-tiles antoi))
      (do-all-other-players (player)
        (let* ((mintoi (make-instance 'rs:mintoi :tile tile
                                                 :taken-from player)))
          (verify-tiles mintoi)
          (is eq player (rs:taken-from mintoi)))))))

(define-test set-toitsu-negative
  (fail (make-instance 'rs:antoi :tile 2)
      'rs:invalid-set-element)
  (fail (make-instance 'rs:mintoi :tile 2 :taken-from :kamicha)
      'rs:invalid-set-element)
  (fail (make-instance 'rs:mintoi :tile (rt:make-tile "1p") :taken-from 2)
      'rs:invalid-tile-taken-from))

(define-test set-shuntsu-positive
  (do-all-suited-tiles (rank suit tile)
    (flet ((verify-tiles (set)
             (let ((tiles (rs:tiles set)))
               (is = 3 (length tiles))
               (true (rt:tile= tile (first tiles)))
               (let ((tile-2 (second tiles)))
                 (is eq (rt:suit tile) (rt:suit tile-2))
                 (is = (+ (rt:rank tile) 1) (rt:rank tile-2)))
               (let ((tile-3 (third tiles)))
                 (is eq (rt:suit tile) (rt:suit tile-3))
                 (is = (+ (rt:rank tile) 2) (rt:rank tile-3))))))
      (when (<= rank 7)
        (let* ((anjun (make-instance 'rs:anjun :lowest-tile tile)))
          (verify-tiles anjun)))
      (when (<= rank 7)
        (do-all-shuntsu-tiles (tile new-tile)
          (let ((minjun (make-instance 'rs:minjun :lowest-tile tile
                                                  :open-tile new-tile
                                                  :taken-from :kamicha)))
            (verify-tiles minjun)
            (is eq new-tile (rs:open-tile minjun))
            (is eq :kamicha (rs:taken-from minjun))))))))

(define-test set-shuntsu-negative
  (fail (make-instance 'rs:anjun :lowest-tile 2)
      'rs:invalid-set-element)
  (fail (make-instance 'rs:minjun :lowest-tile 2
                                  :open-tile 2
                                  :taken-from :kamicha)
      'rs:invalid-set-element)
  (fail (make-instance 'rs:minjun :lowest-tile (rt:make-tile "1p")
                                  :open-tile (rt:make-tile "1p")
                                  :taken-from 2)
      'rs:invalid-tile-taken-from)
  (fail (make-instance 'rs:minjun :lowest-tile (rt:make-tile "1p")
                                  :open-tile (rt:make-tile "4p")
                                  :taken-from :kamicha)
      'rs:open-tile-not-in-set)
  (do-all-honor-tiles (kind tile)
    (fail (make-instance 'rs:anjun :lowest-tile tile) 'rs:invalid-shuntsu))
  (do-all-suited-tiles (rank suit tile)
    (if (> rank 7)
        (fail (make-instance 'rs:anjun :lowest-tile tile)
            'rs:invalid-shuntsu)
        (do-all-honor-tiles (kind honor-tile)
          (fail (make-instance 'rs:minjun :lowest-tile tile
                                          :open-tile honor-tile
                                          :taken-from :kamicha)
              'rs:open-tile-not-in-set)))))

(define-test set-koutsu-positive
  (do-all-tiles (tile)
    (flet ((verify-tiles (set)
             (let ((tiles (rs:tiles set)))
               (is = 3 (length tiles))
               (true (every (a:curry #'rt:tile= tile) tiles)))))
      (let* ((ankou (make-instance 'rs:ankou :tile tile)))
        (verify-tiles ankou))
      (do-all-other-players (player)
        (let* ((minkou (make-instance 'rs:minkou :tile tile
                                                 :taken-from player)))
          (verify-tiles minkou)
          (is eq player (rs:taken-from minkou)))))))

(define-test set-koutsu-negative
  (fail (make-instance 'rs:ankou :tile 2)
      'rs:invalid-set-element)
  (fail (make-instance 'rs:minkou :tile 2 :taken-from :kamicha)
      'rs:invalid-set-element)
  (fail (make-instance 'rs:minkou :tile (rt:make-tile "1p") :taken-from 2)
      'rs:invalid-tile-taken-from))

(define-test set-kantsu-positive
  (do-all-tiles (tile)
    (flet ((verify-tiles (set)
             (let ((tiles (rs:tiles set)))
               (is = 4 (length tiles))
               (true (every (a:curry #'rt:tile= tile) tiles)))))
      (let* ((ankan (make-instance 'rs:ankan :tile tile)))
        (verify-tiles ankan))
      (do-all-other-players (player)
        (let* ((daiminkan (make-instance 'rs:daiminkan :tile tile
                                                       :taken-from player)))
          (verify-tiles daiminkan)
          (is eq player (rs:taken-from daiminkan)))
        (let* ((shouminkan (make-instance 'rs:shouminkan :tile tile
                                                         :taken-from player)))
          (verify-tiles shouminkan)
          (is eq player (rs:taken-from shouminkan)))))))

(define-test set-kantsu-negative
  (fail (make-instance 'rs:ankan :tile 2)
      'rs:invalid-set-element)
  (fail (make-instance 'rs:daiminkan :tile 2 :taken-from :kamicha)
      'rs:invalid-set-element)
  (fail (make-instance 'rs:daiminkan :tile (rt:make-tile "1p") :taken-from 2)
      'rs:invalid-tile-taken-from)
  (fail (make-instance 'rs:shouminkan :tile 2 :taken-from :kamicha)
      'rs:invalid-set-element)
  (fail (make-instance 'rs:shouminkan :tile (rt:make-tile "1p") :taken-from 2)
      'rs:invalid-tile-taken-from))

;;; Set list reader and print

(define-test print-read-set-positive
  (flet ((test (strings &rest args)
           (dolist (string strings)
             (let* ((set (apply #'make-instance args)))
               (let ((result-string (rs:print-set set nil)))
                 (is string= (first strings) result-string
                     "string=: ~A ~A" string result-string))
               (let ((result-set (rs:read-set-from-string string)))
                 (is rs:set= set result-set
                     "set=: ~A ~A" set result-set))))))
    ;; Toitsu
    (test '("66p" "6p6p") 'rs:antoi
          :tile (rt:make-tile "6p"))
    (test '("6*6p" "6*p6p") 'rs:mintoi
          :tile (rt:make-tile "6p")
          :taken-from :kamicha)
    (test '("6*6*p" "6*p6*p") 'rs:mintoi
          :tile (rt:make-tile "6p")
          :taken-from :toimen)
    (test '("66*p" "6p6*p") 'rs:mintoi
          :tile (rt:make-tile "6p")
          :taken-from :shimocha)
    (test '("66z" "6z6z") 'rs:antoi
          :tile (rt:make-tile "6z"))
    (test '("6*6z" "6*z6z") 'rs:mintoi
          :tile (rt:make-tile "6z")
          :taken-from :kamicha)
    (test '("6*6*z" "6*z6*z") 'rs:mintoi
          :tile (rt:make-tile "6z")
          :taken-from :toimen)
    (test '("66*z" "6z6*z") 'rs:mintoi
          :tile (rt:make-tile "6z")
          :taken-from :shimocha)
    ;; Koutsu
    (test '("666p" "6p6p6p" "66p6p" "6p66p") 'rs:ankou
          :tile (rt:make-tile "6p"))
    (test '("6*66p" "6*p6p6p" "6*6p6p" "6*p66p") 'rs:minkou
          :tile (rt:make-tile "6p")
          :taken-from :kamicha)
    (test '("66*6p" "6p6*p6p" "66*p6p" "6p6*6p") 'rs:minkou
          :tile (rt:make-tile "6p")
          :taken-from :toimen)
    (test '("666*p" "6p6p6*p" "66p6*p" "6p66*p") 'rs:minkou
          :tile (rt:make-tile "6p")
          :taken-from :shimocha)
    (test '("666z" "6z6z6z" "66z6z" "6z66z") 'rs:ankou
          :tile (rt:make-tile "6z"))
    (test '("6*66z" "6*z6z6z" "6*6z6z" "6*z66z") 'rs:minkou
          :tile (rt:make-tile "6z")
          :taken-from :kamicha)
    (test '("66*6z" "6z6*z6z" "66*z6z" "6z6*6z") 'rs:minkou
          :tile (rt:make-tile "6z")
          :taken-from :toimen)
    (test '("666*z" "6z6z6*z" "66z6*z" "6z66*z") 'rs:minkou
          :tile (rt:make-tile "6z")
          :taken-from :shimocha)
    ;; Kantsu
    (test '("6666p" "6p666p" "66p66p" "666p6p"
            "6p6p66p" "6p66p6p" "66p6p6p" "6p6p6p6p")
          'rs:ankan
          :tile (rt:make-tile "6p"))
    (test '("6*666p" "6*p666p" "6*6p66p" "6*66p6p"
            "6*p6p66p" "6*p66p6p" "6*6p6p6p" "6*p6p6p6p")
          'rs:daiminkan
          :tile (rt:make-tile "6p")
          :taken-from :kamicha)
    (test '("66*66p" "6p6*66p" "66*p66p" "66*6p6p"
            "6p6*p66p" "6p6*6p6p" "66*p6p6p" "6p6*p6p6p")
          'rs:daiminkan
          :tile (rt:make-tile "6p")
          :taken-from :toimen)
    (test '("6666*p" "6p666*p" "66p66*p" "666p6*p"
            "6p6p66*p" "6p66p6*p" "66p6p6*p" "6p6p6p6*p")
          'rs:daiminkan
          :tile (rt:make-tile "6p")
          :taken-from :shimocha)
    (test '("6*6**66p" "6*p6**66p" "6*6**p66p" "6*6**6p6p"
            "6*p6**p66p" "6*p6**6p6p" "6*6**p6p6p" "6*p6**p6p6p")
          'rs:shouminkan
          :tile (rt:make-tile "6p")
          :taken-from :kamicha)
    (test '("66*6**6p" "6p6*6**6p" "66*p6**6p" "66*6**p6p"
            "6p6*p6**6p" "6p6*6**p6p" "66*p6**p6p" "6p6*p6**p6p")
          'rs:shouminkan
          :tile (rt:make-tile "6p")
          :taken-from :toimen)
    (test '("666*6**p" "6p66*6**p" "66p6*6**p" "666*p6**p"
            "6p6p6*6**p" "6p66*p6**p" "66p6*p6**p" "6p6p6*p6**p")
          'rs:shouminkan
          :tile (rt:make-tile "6p")
          :taken-from :shimocha)
    (test '("6666z" "6z666z" "66z66z" "666z6z"
            "6z6z66z" "6z66z6z" "66z6z6z" "6z6z6z6z")
          'rs:ankan
          :tile (rt:make-tile "6z"))
    (test '("6*666z" "6*z666z" "6*6z66z" "6*66z6z"
            "6*z6z66z" "6*z66z6z" "6*6z6z6z" "6*z6z6z6z")
          'rs:daiminkan
          :tile (rt:make-tile "6z")
          :taken-from :kamicha)
    (test '("66*66z" "6z6*66z" "66*z66z" "66*6z6z"
            "6z6*z66z" "6z6*6z6z" "66*z6z6z" "6z6*z6z6z")
          'rs:daiminkan
          :tile (rt:make-tile "6z")
          :taken-from :toimen)
    (test '("6666*z" "6z666*z" "66z66*z" "666z6*z"
            "6z6z66*z" "6z66z6*z" "66z6z6*z" "6z6z6z6*z")
          'rs:daiminkan
          :tile (rt:make-tile "6z")
          :taken-from :shimocha)
    (test '("6*6**66z" "6*z6**66z" "6*6**z66z" "6*6**6z6z"
            "6*z6**z66z" "6*z6**6z6z" "6*6**z6z6z" "6*z6**z6z6z")
          'rs:shouminkan
          :tile (rt:make-tile "6z")
          :taken-from :kamicha)
    (test '("66*6**6z" "6z6*6**6z" "66*z6**6z" "66*6**z6z"
            "6z6*z6**6z" "6z6*6**z6z" "66*z6**z6z" "6z6*z6**z6z")
          'rs:shouminkan
          :tile (rt:make-tile "6z")
          :taken-from :toimen)
    (test '("666*6**z" "6z66*6**z" "66z6*6**z" "666*z6**z"
            "6z6z6*6**z" "6z66*z6**z" "66z6*z6**z" "6z6z6*z6**z")
          'rs:shouminkan
          :tile (rt:make-tile "6z")
          :taken-from :shimocha)
    ;; Shuntsu
    (test '("123p" "132p" "213p" "231p" "312p" "321p"
            "1p23p" "1p32p" "2p13p" "2p31p" "3p12p" "3p21p"
            "12p3p" "13p2p" "21p3p" "23p1p" "31p2p" "32p1p"
            "1p2p3p" "1p3p2p" "2p1p3p" "2p3p1p" "3p1p2p" "3p2p1p")
          'rs:anjun
          :lowest-tile (rt:make-tile "1p"))
    (test '("1*23p" "1*p23p" "1*2p3p" "1*p2p3p"
            "1*32p" "1*p32p" "1*3p2p" "1*p3p2p")
          'rs:minjun
          :taken-from :kamicha
          :lowest-tile (rt:make-tile "1p")
          :open-tile (rt:make-tile "1p"))
    (test '("2*13p" "2*p13p" "2*1p3p" "2*p1p3p"
            "2*31p" "2*p31p" "2*3p1p" "2*p3p1p")
          'rs:minjun
          :taken-from :kamicha
          :lowest-tile (rt:make-tile "1p")
          :open-tile (rt:make-tile "2p"))
    (test '("3*12p" "3*p12p" "3*1p2p" "3*p1p2p"
            "3*21p" "3*p21p" "3*2p1p" "3*p2p1p")
          'rs:minjun
          :taken-from :kamicha
          :lowest-tile (rt:make-tile "1p")
          :open-tile (rt:make-tile "3p"))
    (test '("21*3p" "2p1*3p" "21*p3p" "2p1*p3p"
            "31*2p" "3p1*2p" "31*p2p" "3p1*p2p")
          'rs:minjun
          :taken-from :toimen
          :lowest-tile (rt:make-tile "1p")
          :open-tile (rt:make-tile "1p"))
    (test '("12*3p" "1p2*3p" "12*p3p" "1p2*p3p"
            "32*1p" "3p2*1p" "32*p1p" "3p2*p1p")
          'rs:minjun
          :taken-from :toimen
          :lowest-tile (rt:make-tile "1p")
          :open-tile (rt:make-tile "2p"))
    (test '("13*2p" "1p3*2p" "13*p2p" "1p3*p2p"
            "23*1p" "2p3*1p" "23*p1p" "2p3*p1p")
          'rs:minjun
          :taken-from :toimen
          :lowest-tile (rt:make-tile "1p")
          :open-tile (rt:make-tile "3p"))
    (test '("231*p" "2p31*p" "23p1*p" "2p3p1*p"
            "321*p" "3p21*p" "32p1*p" "3p2p1*p")
          'rs:minjun
          :taken-from :shimocha
          :lowest-tile (rt:make-tile "1p")
          :open-tile (rt:make-tile "1p"))
    (test '("132*p" "1p32*p" "13p2*p" "1p3p2*p"
            "312*p" "3p12*p" "31p2*p" "3p1p2*p")
          'rs:minjun
          :taken-from :shimocha
          :lowest-tile (rt:make-tile "1p")
          :open-tile (rt:make-tile "2p"))
    (test '("123*p" "1p23*p" "12p3*p" "1p2p3*p"
            "213*p" "2p13*p" "21p3*p" "2p1p3*p")
          'rs:minjun
          :taken-from :shimocha
          :lowest-tile (rt:make-tile "1p")
          :open-tile (rt:make-tile "3p"))))

(define-test read-set-negative
  (flet ((test (string)
           (fail (rs:read-set-from-string string) 'rs:set-reader-error)))
    (map nil #'test
         '("124p" "boo" "  " "3p" "12p" "13p" "123q"
           "111*1z" "" "123456789m123456789p123456789s1234567z"
           "012p" "890p" "000p" "123z"))))

(define-test set=
  ;; Slow (20+ minutes on my machine), exhaustive correctness test for SET=.
  ;; Uncomment and run when needed.
  #+(or)
  (let ((i 0))
    (do-all-valid-sets (set-1)
      (do-all-valid-sets (set-2)
        (incf i)
        (when (= 0 (mod i 10000))
          (princ ".")
          (finish-output))
        (if (and (eq (class-of set-1) (class-of set-2))
                 (rt:tile-list= (rs:tiles set-1) (rs:tiles set-2))
                 (if (typep set-1 'rs:minjun)
                     (rt:tile= (rs:open-tile set-1) (rs:open-tile set-2))
                     t))
            (is rs:set= set-1 set-2)
            (isnt rs:set= set-1 set-2))))
    (fresh-line)))

;;; Tile-set matcher tests
;;; NOTE: we do not test any kans, since they must always be declared and are
;;; therefore only allowed in the locked sets list.

(defun test-make-set (&key tiles winning-tile win-from forbidden-sets
                        expected-set expected-tiles expected-winning-tile)
  (multiple-value-bind (actual-set actual-tiles actual-winning-tile)
      (rs:try-make-set-from-tiles tiles winning-tile win-from forbidden-sets)
    (if (null expected-set)
        (true (null actual-set))
        (is rs:set= expected-set actual-set))
    (is rt:tile-list= expected-tiles actual-tiles)
    (if (null expected-winning-tile)
        (true (null actual-winning-tile))
        (is rt:tile= expected-winning-tile actual-winning-tile))))

;; TODO: wait for IS-VALUES to be fixed.
;; TODO: tests for a NIL winning tile - e.g. for when it was already consumed
;;       by other matchers.
(define-test try-make-set-antoi
  ;; 2p + 3p → ∅
  (test-make-set
   :tiles '([2p]) :winning-tile [3p] :win-from :tsumo
   :forbidden-sets '()
   :expected-set nil
   :expected-tiles '([2p])
   :expected-winning-tile [3p])
  ;; 2p + 2p → 22p
  (test-make-set
   :tiles '([2p]) :winning-tile [2p] :win-from :tsumo
   :forbidden-sets '()
   :expected-set (rs:antoi [2p])
   :expected-tiles '()
   :expected-winning-tile nil)
  ;; 22p + 3p → 22p
  (test-make-set
   :tiles '([2p] [2p]) :winning-tile [3p] :win-from :tsumo
   :forbidden-sets '()
   :expected-set (rs:antoi [2p])
   :expected-tiles '()
   :expected-winning-tile [3p])
  ;; 23p + 3p → 33p
  (test-make-set
   :tiles '([2p] [3p]) :winning-tile [3p] :win-from :tsumo
   :forbidden-sets '()
   :expected-set (rs:antoi [3p])
   :expected-tiles '([2p])
   :expected-winning-tile nil)
  ;; Kokushi musou pair search
  (let ((tiles '([1m] [9m] [1p] [9p] [1s] [9s]
                 [EW] [SW] [WW] [NW] [WD] [GD] [RD])))
    (dolist (tile tiles)
      (test-make-set
       :tiles tiles :winning-tile tile :win-from :tsumo
       :forbidden-sets '()
       :expected-set (rs:antoi tile)
       :expected-tiles (remove tile tiles :test #'rt:tile=)
       :expected-winning-tile nil))))

(define-test try-make-set-mintoi
  (do-all-other-players (player)
    ;; 2p + 3p → ∅
    (test-make-set
     :tiles '([2p]) :winning-tile [3p] :win-from player
     :forbidden-sets '()
     :expected-set nil
     :expected-tiles '([2p])
     :expected-winning-tile [3p])
    ;; 2p + 2p → 22p
    (test-make-set
     :tiles '([2p]) :winning-tile [2p] :win-from player
     :forbidden-sets '()
     :expected-set (rs:mintoi [2p] player)
     :expected-tiles '()
     :expected-winning-tile nil)
    ;; 22p + 3p → 22p
    (test-make-set
     :tiles '([2p] [2p]) :winning-tile [3p] :win-from player
     :forbidden-sets '()
     ;; NOTE: antoi, not mintoi - we do not use the winning tile
     :expected-set (rs:antoi [2p])
     :expected-tiles '()
     :expected-winning-tile [3p])
    ;; 23p + 3p → 33p
    (test-make-set
     :tiles '([2p] [3p]) :winning-tile [3p] :win-from player
     :forbidden-sets '()
     :expected-set (rs:mintoi [3p] player)
     :expected-tiles '([2p])
     :expected-winning-tile nil)
    ;; Kokushi musou pair search
    (let ((tiles '([1m] [9m] [1p] [9p] [1s] [9s]
                   [EW] [SW] [WW] [NW] [WD] [GD] [RD])))
      (dolist (tile tiles)
        (test-make-set
         :tiles tiles :winning-tile tile :win-from player
         :forbidden-sets '()
         :expected-set (rs:mintoi tile player)
         :expected-tiles (remove tile tiles :test #'rt:tile=)
         :expected-winning-tile nil)))))

(define-test try-make-set-ankou
  ;; 22p + 3p → ∅
  (test-make-set
   :tiles '([2p] [2p]) :winning-tile [3p] :win-from :tsumo
   :forbidden-sets (list (rs:antoi [2p]))
   :expected-set nil
   :expected-tiles '([2p] [2p])
   :expected-winning-tile [3p])
  ;; 22p + 2p → 222p
  (test-make-set
   :tiles '([2p] [2p]) :winning-tile [2p] :win-from :tsumo
   :forbidden-sets (list (rs:antoi [2p]))
   :expected-set (rs:ankou [2p])
   :expected-tiles '()
   :expected-winning-tile nil)
  ;; 222p + 3p → 222p
  (test-make-set
   :tiles '([2p] [2p] [2p]) :winning-tile [3p] :win-from :tsumo
   :forbidden-sets (list (rs:antoi [2p]))
   :expected-set (rs:ankou [2p])
   :expected-tiles '()
   :expected-winning-tile [3p])
  ;; 233p + 3p → 333p
  (test-make-set
   :tiles '([2p] [3p] [3p]) :winning-tile [3p] :win-from :tsumo
   :forbidden-sets '()
   :expected-set (rs:ankou [3p])
   :expected-tiles '([2p])
   :expected-winning-tile nil)
  ;; Chiitoitsu tenpai triplet search
  (let ((hand '([1m] [1m] [1p] [1p] [1s] [1s]
                [EW] [EW] [SW] [SW] [WW] [WW] [NW]))
        (tiles '([1m] [1p] [1s] [EW] [SW] [WW])))
    (dolist (tile tiles)
      (test-make-set
       :tiles hand :winning-tile tile :win-from :tsumo
       :forbidden-sets (mapcar #'rs:antoi tiles)
       :expected-set (rs:ankou tile)
       :expected-tiles (remove tile hand :count 2 :test #'rt:tile=)
       :expected-winning-tile nil))))

(define-test try-make-set-minkou
  (do-all-other-players (player)
    ;; 22p + 3p → ∅
    (test-make-set
     :tiles '([2p] [2p]) :winning-tile [3p] :win-from player
     :forbidden-sets (list (rs:antoi [2p]))
     :expected-set nil
     :expected-tiles '([2p] [2p])
     :expected-winning-tile [3p])
    ;; 22p + 2p → 222p
    (test-make-set
     :tiles '([2p] [2p]) :winning-tile [2p] :win-from player
     :forbidden-sets (list (rs:antoi [2p]))
     :expected-set (rs:minkou [2p] player)
     :expected-tiles '()
     :expected-winning-tile nil)
    ;; 222p + 3p → 222p
    (test-make-set
     :tiles '([2p] [2p] [2p]) :winning-tile [3p] :win-from player
     :forbidden-sets (list (rs:antoi [2p]))
     ;; NOTE: ankou, not minkou - we do not use the winning tile
     :expected-set (rs:ankou [2p])
     :expected-tiles '()
     :expected-winning-tile [3p])
    ;; 233p + 3p → 333p
    (test-make-set
     :tiles '([2p] [3p] [3p]) :winning-tile [3p] :win-from player
     :forbidden-sets '()
     :expected-set (rs:minkou [3p] player)
     :expected-tiles '([2p])
     :expected-winning-tile nil)
    ;; Chiitoitsu tenpai triplet search
    (let ((hand '([1m] [1m] [1p] [1p] [1s] [1s]
                  [EW] [EW] [SW] [SW] [WW] [WW] [NW]))
          (tiles '([1m] [1p] [1s] [EW] [SW] [WW])))
      (dolist (tile tiles)
        (test-make-set
         :tiles hand :winning-tile tile :win-from player
         :forbidden-sets (mapcar (a:rcurry #'rs:antoi) tiles)
         :expected-set (rs:minkou tile player)
         :expected-tiles (remove tile hand :count 2 :test #'rt:tile=)
         :expected-winning-tile nil)))))

(define-test try-make-set-anjun
  ;; 25p + 3p → ∅
  (test-make-set
   :tiles '([2p] [5p]) :winning-tile [3p] :win-from :tsumo
   :forbidden-sets '()
   :expected-set nil
   :expected-tiles '([2p] [5p])
   :expected-winning-tile [3p])
  ;; 24p + 3p → 234p
  (test-make-set
   :tiles '([2p] [4p]) :winning-tile [3p] :win-from :tsumo
   :forbidden-sets '()
   :expected-set (rs:anjun [2p])
   :expected-tiles '()
   :expected-winning-tile nil)
  ;; 23p + 4p → 234p
  (test-make-set
   :tiles '([2p] [3p]) :winning-tile [4p] :win-from :tsumo
   :forbidden-sets '()
   :expected-set (rs:anjun [2p])
   :expected-tiles '()
   :expected-winning-tile nil)
  ;; 122p + 3p → 123p
  (test-make-set
   :tiles '([1p] [2p] [2p]) :winning-tile [3p] :win-from :tsumo
   :forbidden-sets (list (rs:antoi [2p]))
   :expected-set (rs:anjun [1p])
   :expected-tiles '([2p])
   :expected-winning-tile nil)
  ;; 234p + 6p → 234p
  (test-make-set
   :tiles '([2p] [3p] [4p]) :winning-tile [6p] :win-from :tsumo
   :forbidden-sets '()
   :expected-set (rs:anjun [2p])
   :expected-tiles '()
   :expected-winning-tile [6p]))

(define-test try-make-set-minjun
  (do-all-other-players (player)
    ;; 25p + 3p → ∅
    (test-make-set
     :tiles '([2p] [5p]) :winning-tile [3p] :win-from player
     :forbidden-sets '()
     :expected-set nil
     :expected-tiles '([2p] [5p])
     :expected-winning-tile [3p])
    ;; 24p + 3p → 234p
    (test-make-set
     :tiles '([2p] [4p]) :winning-tile [3p] :win-from player
     :forbidden-sets '()
     :expected-set (rs:minjun [2p] [3p] player)
     :expected-tiles '()
     :expected-winning-tile nil)
    ;; 23p + 4p → 234p
    (test-make-set
     :tiles '([2p] [3p]) :winning-tile [4p] :win-from player
     :forbidden-sets '()
     :expected-set (rs:minjun [2p] [4p] player)
     :expected-tiles '()
     :expected-winning-tile nil)
    ;; 122p + 3p → 123p
    (test-make-set
     :tiles '([1p] [2p] [2p]) :winning-tile [3p] :win-from player
     :forbidden-sets (list (rs:antoi [2p]))
     :expected-set (rs:minjun [1p] [3p] player)
     :expected-tiles '([2p])
     :expected-winning-tile nil)
    ;; 234p + 6p → 234p
    (test-make-set
     :tiles '([2p] [3p] [4p]) :winning-tile [6p] :win-from :tsumo
     :forbidden-sets '()
     ;; NOTE: ankou, not minkou - we do not use the winning tile
     :expected-set (rs:anjun [2p])
     :expected-tiles '()
     :expected-winning-tile [6p])))
