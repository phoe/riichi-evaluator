;;;; riichi-evaluator.asd

(asdf:defsystem #:riichi-evaluator
  :description "A utility for evaluating Riichi Mahjong hands"
  :author "Kimmo \"keko\" Kenttälä and Michał \"phoe\" Herda"
  :maintainer "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :depends-on (#:alexandria
               #:named-readtables
               #:protest/base)
  :components ((:file "tiles")
               (:file "set")
               (:file "hand")
               ;;(:file "mah-eval")
               ))

(defmethod perform ((o test-op) (c (eql (find-system :riichi-evaluator))))
  (load-system :riichi-evaluator/test)
  (symbol-call :parachute :test :riichi-evaluator/test))

(asdf:defsystem #:riichi-evaluator/test
  :description "Tests for Riichi Mahjong evaluator"
  :author "Kimmo \"keko\" Kenttälä and Michał \"phoe\" Herda"
  :maintainer "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "t"
  :depends-on (#:alexandria
               #:parachute
               #:named-readtables)
  :components ((:file "package")
               (:file "tiles")
               (:file "set")
               (:file "hand")))
