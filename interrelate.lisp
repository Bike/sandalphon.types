(in-package #:sandalphon.types)

(defmacro defdisjoint (tc1 tc2)
  `(progn
     (defmethod subtypep tri/combine ((t1 ,tc1) (t2 ,tc2))
       (values nil t))
     (defmethod subtypep tri/combine ((t1 ,tc2) (t2 ,tc1))
       (values nil t))
     (defmethod conjoin/2 ((t1 ,tc1) (t2 ,tc2)) *the-type-nil*)))

(macrolet ((disjoint-kinds (&rest type-classes)
	     (let (forms)
	       (map-combinations
		(lambda (tcs)
		  (push `(defdisjoint ,@tcs) forms))
		type-classes :length 2)
	       `(progn ,@forms))))
  (disjoint-kinds array-type cons-type interval-type))

;; some types have infinitely many members, so they're never
;;  subtypes of eql or member types.
(defmacro definfinite (tc)
  `(progn
     (defmethod subtypep tri/combine
         ((t1 ,tc) (t2 eql-type))
       (values nil t))
     (defmethod subtypep tri/combine
         ((t1 ,tc) (t2 member-type))
       (values nil t))))

(definfinite array-type)
(definfinite cons-type)
;; interval is more complicated

(defmacro deftypeclass (tc classname)
  `(progn
     (defmethod subtypep tri/combine ((t1 ,tc) (t2 class))
       ;; e.g., class ARRAY is not a subclass of class SIMPLE-BIT-VECTOR
       ;; but (simple-array bit (*)) is.
       (tri/if (cl:subtypep (find-class ',classname) t2)
	       (values t t)
	       (values nil nil)
	       (values nil nil)))
     (defmethod subtypep tri/combine ((t1 class) (t2 ,tc))
       (tri/if (cl:subtypep t1 (find-class ',classname))
	       (values nil nil)
	       (values nil t)
	       (values nil nil)))))

(deftypeclass cons-type cons)
;; FIXME: we can do better on these two, at least for CL-defined classes
(deftypeclass array-type array)
(deftypeclass interval-type real)
