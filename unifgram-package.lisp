(cl:in-package :cl)

(defpackage :unifgram
  (:use :cl :paiprolog :paiprolog.auxfns)
  (:shadowing-import-from :paiprolog.auxfns #:symbol #:debug)
  (:export #:rule
           #:-->
           #:--->
           #:==>
           #:add*))
