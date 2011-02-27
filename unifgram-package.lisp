(macrolet ((m ()
             `(defpackage #:unifgram
                (:use #:cl #:paiprolog)
                (:shadowing-import-from
                 #:paiprolog
                 ,@(loop for sym being the present-symbols in :paiprolog
                         collect sym))
                (:export #:rule
                         #:-->
                         #:==>
                         #:add*))))
  (m))
