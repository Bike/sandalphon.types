(asdf:defsystem #:sandalphon.types
  :description "More fuckwithable CL type system."
  :author "Bike <aeshtaer@gmail.com>"
  :license "WTFPL"
  :version "0.1"
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "trivalent" :depends-on ("package"))
	       (:file "generics"
		:depends-on ("trivalent" "package"))
	       (:file "CLOS"
		:depends-on ("generics" "package"))
	       (:file "universe"
		:depends-on ("generics" "package"))
	       (:file "array"
		:depends-on ("generics" "package"))
	       (:file "cons"
		:depends-on ("generics" "universe"
					"package"))
	       (:file "real"
		:depends-on ("generics" "universe"
					"eql-member" "package"))
	       (:file "eql-member"
		:depends-on ("universe" "generics" "package"))
	       (:file "satisfies"
		:depends-on ("generics" "package"))
	       (:file "logical"
		:depends-on ("generics" "universe" "package"))
	       (:file "interrelate"
		:depends-on ("generics" "cons" "real"
					"eql-member"
					"universe" "array"
					"trivalent" "package"))))
