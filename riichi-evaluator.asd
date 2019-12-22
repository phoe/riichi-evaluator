;;;; riichi-evaluator.asd

(asdf:defsystem #:riichi-evaluator
  :description "A utility for evaluating Riichi Mahjong hands"
  :author "Kimmo \"keko\" Kenttälä and Michał \"phoe\" Herda"
  :maintainer "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:named-readtables
               #:protest/base)
  :components ((:file "tiles")
               (:file "set")
               (:file "hand")
               ;;(:file "mah-eval")
               ))
