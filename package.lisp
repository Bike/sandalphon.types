(defpackage #:sandalphon.types
  (:use #:cl #:alexandria)
  (:shadow #:subtypep #:typep #:type)
  (:shadow #:conjoin #:disjoin #:type=)
  (:export #:typep #:subtypep #:conjoin #:disjoin #:negate
	   #:tri/combine ; for the method combo, but what of others
	   #:conjoin/2 #:disjoin/2
	   #:type=)
  (:export #:parse-type #:typexpand-1 #:typexpand
	   #:specifier-symbol-macro #:specifier-macro
	   #:specifier-variable #:specifier-function
	   #:specifier-special
	   #:deftype-symbol-macro #:deftype-macro
	   #:deftype-variable #:deftype-function)
  (:export #:unparse))
