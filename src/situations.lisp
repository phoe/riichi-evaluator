;;;; src/situations.lisp
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

;; TODO package definition here

;; TODO move into yaku definitions
;; TODO invalid-situation tests

(defmethod validate-situation progn
    (hand situation &rest args)
  ;; TODO: In case of no riichi, verify that the list of ura doras is empty.
  ;; How exactly do we achieve that? Dunno. Probably here.
  (when (null (compute-applicable-methods
               #'validate-situation (list* hand situation args)))
    (invalid-situation hand situation args "Unknown situation ~S." situation)))

;;; Riichi

(defmethod validate-situation progn
    (hand (situation (eql :riichi)) &rest args)
  ;; TODO: in case of riichi, verify that the list of ura doras is as long as
  ;; the list of doras.
  (unless (null args)
    (invalid-situation hand situation args
                       "Riichi does not accept arguments.")))

(defmethod validate-situation progn
    ((hand open-hand) (situation (eql :riichi)) &rest args)
  (invalid-situation hand situation args
                     "Riichi cannot be declared on an open hand."))

;;; Double riichi

(defmethod validate-situation progn
    (hand (situation (eql :double-riichi)) &rest args)
  (unless (member :riichi (situations hand))
    (invalid-situation hand situation args
                       "Double riichi cannot occur without riichi."))
  (unless (null args)
    (invalid-situation hand situation args
                       "Double riichi does not accept arguments.")))

;;; Open riichi

(defmethod validate-situation progn
    (hand (situation (eql :open-riichi)) &rest args)
  (unless (member :riichi (situations hand))
    (invalid-situation hand situation args
                       "Open riichi cannot occur without riichi."))
  (unless (null args)
    (invalid-situation hand situation args
                       "Open riichi does not accept arguments.")))

;;; Ippatsu

(defmethod validate-situation progn
    (hand (situation (eql :ippatsu)) &rest args)
  (unless (member :riichi (situations hand))
    (invalid-situation hand situation args
                       "Ippatsu cannot occur without riichi."))
  (unless (null args)
    (invalid-situation hand situation args
                       "Ippatsu does not accept arguments.")))
