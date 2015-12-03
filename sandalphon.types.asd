(asdf:defsystem #:sandalphon.types
  :description "More fuckwithable CL type system."
  :author "Bike <aeshtaer@gmail.com>"
  :license "WTFPL"
  :version "0.1"
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "parse" :depends-on ("package"))
	       (:file "unparse" :depends-on ("generics" "package"))
	       (:file "trivalent" :depends-on ("package"))
	       (:file "generics"
		      :depends-on ("trivalent" "package"))
	       (:file "universe"
		      :depends-on ("generics" "parse" "unparse"
					      "package"))
	       (:file "array"
		      :depends-on ("generics" "parse" "unparse"
					      "package"))
	       (:file "cons"
		      :depends-on ("generics" "universe"
					      "parse" "unparse"
					      "package"))
	       (:file "real"
		      :depends-on ("generics" "universe"
				   "eql-member" "package"))
	       (:file "eql-member"
		      :depends-on ("universe"
				   "generics" "parse" "unparse"
				   "package"))
	       (:file "logical"
		      :depends-on ("generics" "universe"
					      "parse" "unparse"
					      "package"))))
