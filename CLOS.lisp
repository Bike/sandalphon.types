(in-package #:sandalphon.types)

;;; We just piggyback on CLOS classes, which are reified types anyway.
;;; there are some fiddly bits with built-in-classes,
;;;  like (subtypep 'cons (find-class 'cons)) should be T T.

(defmethod typep (obj (type class))
  (cl:typep obj type))

(defmethod subtypep tri/combine ((t1 class) (t2 class))
  (cl:subtypep t1 t2))

(defmethod unparse ((type class)) (class-name type))
