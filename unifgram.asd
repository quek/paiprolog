(asdf:defsystem #:unifgram
  :serial t
  :components ((:file "unifgram-package")
               (:file "unifgram"))
  :depends-on (#:paiprolog))
