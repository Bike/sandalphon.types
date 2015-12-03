(in-package #:sandalphon.types)

(defclass top-type (type) ())

(defclass bottom-type (type) ())

(defmethod typep (obj (type top-type)) t)
(defmethod typep (obj (type bottom-type)) nil)

;; precedence/left-to-right of tri/combine actually matters here
(defmethod subtypep tri/combine ((t1 top-type) (t2 top-type))
  (values t t))

(defmethod subtypep tri/combine ((t1 top-type) t2)
  (values nil t))

(defmethod subtypep tri/combine (t1 (t2 top-type))
  (values t t))

(defmethod subtypep tri/combine ((t1 bottom-type) t2)
  (values t t))

(defmethod subtypep tri/combine (t1 (t2 bottom-type))
  (values nil t))

;; these overlap, but they do the same thing on e.g. (and t nil) regardless of order.
(defmethod-commutative conjoin/2 ((t1 top-type) t2) t2)
(defmethod-commutative conjoin/2 ((t1 bottom-type) t2) t1)
(defmethod-commutative disjoin/2 ((t1 bottom-type) t2) t2)
(defmethod-commutative disjoin/2 ((t1 top-type) t2) t1)

;; defconstant? something?
(defparameter *the-type-t* (make-instance 'top-type))
(defparameter *the-type-nil* (make-instance 'bottom-type))

(deftype-variable t *the-type-t*)
(deftype-variable nil *the-type-nil*)

(defmethod unparse ((type top-type)) 't)
(defmethod unparse ((type bottom-type)) 'nil)
