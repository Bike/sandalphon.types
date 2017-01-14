(in-package #:sandalphon.types)

(defclass satisfies-type (type)
  ((predname :initarg :pred :accessor satisfies-type-predicate-name)))

(defmethod typep (obj (type satisfies-type))
  (funcall (fdefinition (satisfies-type-predicate-name type)) obj))

(defmethod subtypep tri/combine
    ((t1 satisfies-type) (t2 satisfies-type))
  ;; someone could probably make the case that (satisfies foo) is not
  ;;  a subtype of itself if it has side effects and can return
  ;;  differently on the same data, but (a) then the idea of types as
  ;;  sets is fundamentally flawed, and (b) fuck that
  (if (eql (satisfies-type-predicate-name t1)
	   (satisfies-type-predicate-name t2))
      (values t t)
      (values nil nil)))
