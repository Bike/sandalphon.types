(in-package #:sandalphon.types)

(defclass type () ()) ; abstract

(defgeneric typep (object type)
  (:argument-precedence-order type object))

(defgeneric conjoin/2 (t1 t2))
(defgeneric disjoin/2 (t1 t2))

(defgeneric negate (type))

(defun conjoin (&rest types)
  (reduce #'conjoin/2 types :initial-value *the-type-t*))
(defun disjoin (&rest types)
  (reduce #'disjoin/2 types :initial-value *the-type-nil*))

(define-method-combination tri/combine
    :identity-with-one-argument t)

(defgeneric subtypep (t1 t2)
  (:method tri/combine (t1 t2) (values nil nil))
  (:method-combination tri/combine))

;; make generic?
(defun type= (t1 t2)
  (tri/and (subtypep t1 t2)
	   (subtypep t2 t1)))

(defmacro defmethod-commutative (name ((a1 class) a2) &body body)
  `(progn (defmethod ,name ((,a1 ,class) ,a2) ,@body)
	  (defmethod ,name (,a2 (,a1 ,class)) ,@body)))
