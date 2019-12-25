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

;;; Protocol classes

(define-test set-protocol
  (flet ((try (class &rest args)
           (fail (apply #'make-instance class args) 'p:protocol-error)))
    (try 'rs:set 'rs::%count 3)
    (try 'rs:same-tile-set 'rs::%count 3 :tile (rt:make-tile "1p"))
    (try 'rs:closed-set 'rs::%count 3)
    (try 'rs:open-set  'rs::%count 3 :taken-from :kamicha)
    (try 'rs:shuntsu  'rs::%count 3 :lowest-tile (rt:make-tile "1p"))
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
    (when (> rank 7)
      (fail (make-instance 'rs:anjun :lowest-tile tile)
          'rs:invalid-shuntsu))
    (when (<= rank 7)
      (do-all-shuntsu-tiles (tile new-tile)
        (do-all-other-players (player)
          (unless (eq player :kamicha)
            (fail (make-instance 'rs:minjun :lowest-tile tile
                                            :open-tile new-tile
                                            :taken-from player)
                'rs:minjun-invalid-meld))))
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
  (flet ((test (string &rest args)
           (let* ((set (apply #'make-instance args)))
             (let ((result-string (rs:print-set set nil)))
               (is string= string result-string
                   "string=: ~A ~A" string result-string))
             (let ((result-set (rs:read-set-from-string string)))
               (is rs:set= set result-set
                   "set=: ~A ~A" set result-set)))))
    ;; Toitsu
    (test "22p" 'rs:antoi :tile (rt:make-tile "2p"))
    (test "2*2p" 'rs:mintoi :tile (rt:make-tile "2p")
                            :taken-from :kamicha)
    (test "2*2*p" 'rs:mintoi :tile (rt:make-tile "2p")
                             :taken-from :toimen)
    (test "22*p" 'rs:mintoi :tile (rt:make-tile "2p")
                            :taken-from :shimocha)
    (test "22z" 'rs:antoi :tile (rt:make-tile "2z"))
    (test "2*2z" 'rs:mintoi :tile (rt:make-tile "2z")
                            :taken-from :kamicha)
    (test "2*2*z" 'rs:mintoi :tile (rt:make-tile "2z")
                             :taken-from :toimen)
    (test "22*z" 'rs:mintoi :tile (rt:make-tile "2z")
                            :taken-from :shimocha)
    ;; Koutsu
    (test "222p" 'rs:ankou :tile (rt:make-tile "2p"))
    (test "2*22p" 'rs:minkou :tile (rt:make-tile "2p")
                             :taken-from :kamicha)
    (test "22*2p" 'rs:minkou :tile (rt:make-tile "2p")
                             :taken-from :toimen)
    (test "222*p" 'rs:minkou :tile (rt:make-tile "2p")
                             :taken-from :shimocha)
    (test "222z" 'rs:ankou :tile (rt:make-tile "2z"))
    (test "2*22z" 'rs:minkou :tile (rt:make-tile "2z")
                             :taken-from :kamicha)
    (test "22*2z" 'rs:minkou :tile (rt:make-tile "2z")
                             :taken-from :toimen)
    (test "222*z" 'rs:minkou :tile (rt:make-tile "2z")
                             :taken-from :shimocha)
    ;; Kantsu
    (test "2222p" 'rs:ankan :tile (rt:make-tile "2p"))
    (test "2*222p" 'rs:daiminkan :tile (rt:make-tile "2p")
                                 :taken-from :kamicha)
    (test "22*22p" 'rs:daiminkan :tile (rt:make-tile "2p")
                                 :taken-from :toimen)
    (test "2222*p" 'rs:daiminkan :tile (rt:make-tile "2p")
                                 :taken-from :shimocha)
    (test "2*2**22p" 'rs:shouminkan :tile (rt:make-tile "2p")
                                    :taken-from :kamicha)
    (test "22*2**2p" 'rs:shouminkan :tile (rt:make-tile "2p")
                                    :taken-from :toimen)
    (test "222*2**p" 'rs:shouminkan :tile (rt:make-tile "2p")
                                    :taken-from :shimocha)
    (test "2222z" 'rs:ankan :tile (rt:make-tile "2z"))
    (test "2*222z" 'rs:daiminkan :tile (rt:make-tile "2z")
                                 :taken-from :kamicha)
    (test "22*22z" 'rs:daiminkan :tile (rt:make-tile "2z")
                                 :taken-from :toimen)
    (test "2222*z" 'rs:daiminkan :tile (rt:make-tile "2z")
                                 :taken-from :shimocha)
    (test "2*2**22z" 'rs:shouminkan :tile (rt:make-tile "2z")
                                    :taken-from :kamicha)
    (test "22*2**2z" 'rs:shouminkan :tile (rt:make-tile "2z")
                                    :taken-from :toimen)
    (test "222*2**z" 'rs:shouminkan :tile (rt:make-tile "2z")
                                    :taken-from :shimocha)
    ;; Shuntsu
    (test "123p" 'rs:anjun :lowest-tile (rt:make-tile "1p"))
    (test "1*23p" 'rs:minjun :taken-from :kamicha
                             :lowest-tile (rt:make-tile "1p")
                             :open-tile (rt:make-tile "1p"))
    (test "2*13p" 'rs:minjun :taken-from :kamicha
                             :lowest-tile (rt:make-tile "1p")
                             :open-tile (rt:make-tile "2p"))
    (test "3*12p" 'rs:minjun :taken-from :kamicha
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
  ;; Slow, exhaustive correctness test for SET=. Uncomment when needed.
  #+(or)
  (let ((i 0))
    (do-all-valid-sets (set-1)
      (do-all-valid-sets (set-2)
        (incf i)
        (when (= 0 (mod i 10000))
          (princ ".")
          (finish-output))
        (if (and (eq (class-of set-1) (class-of set-2))
                 (every #'rt:tile= (rs:tiles set-1) (rs:tiles set-2))
                 (if (typep set-1 'rs:minjun)
                     (rt:tile= (rs:open-tile set-1) (rs:open-tile set-2))
                     t))
            (is rs:set= set-1 set-2)
            (isnt rs:set= set-1 set-2))))
    (fresh-line)))

(define-test foo
  (is = (+ 2 2) 4))
