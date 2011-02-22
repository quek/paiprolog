(asdf:defsystem paiprolog
    :components ((:file "package")
		 (:file "auxfns" :depends-on ("package"))
		 (:file "patmatch" :depends-on ("package"))
		 (:file "unify" :depends-on ("package" "auxfns" "patmatch"))
		 (:file "prolog" :depends-on ("package" "unify"))
		 (:file "prologc" :depends-on ("package" "prolog"))
		 (:file "prologcp" 
			:depends-on ("package" "auxfns" "prologc"))))
