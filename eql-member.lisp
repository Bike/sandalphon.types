(in-package #:sandalphon.types)

(defclass member-type (type)
  ((objects :initarg :objs :accessor member-type-objects)))

(defmethod typep (object (type member-type))
  (find object (member-type-objects type)))

(defmethod subtypep tri/combine ((t1 member-type) t2)
  "Always certain."
  (values (every (lambda (o) (typep o t2))
		 (member-type-objects t1))
	  t))
;; a <: b with a a non-member non-eql type and b a member or eql
;;  type is almost always false. Particularly, for every standard
;;  type except for nil, null, (satisfies ...), pathologic
;;  numeric types, and equivalents through and/or/not.
;; But since the user can define their own kinds, we can't just
;;  define a method to default to NIL T. Gotta put in all this info
;;  manually. Sucks, huh?

(defmethod conjoin/2 ((t1 member-type) (t2 member-type))
  ;; could check for one being a subset of the other first,
  ;;  to avoid consing up a new instance for no reason.
  (let ((intersection (intersection (member-type-objects t1)
				    (member-type-objects t2))))
    ;; do some easy reductions
    (cond ((null intersection) *the-type-nil*)
	  ((null (rest intersection))
	   (make-instance 'eql-type :obj (first intersection)))
	  (t
	   (make-instance 'member-type :objs intersection)))))

(defmethod disjoin/2 ((t1 member-type) (t2 member-type))
  (let ((union (union (member-type-objects t1)
		      (member-type-objects t2))))
    (make-instance 'member-type :objs union)))

(setf (assoc-value *global-type-environment-specials* 'eql)
      (lambda (spec env)
	(declare (ignore env))
	(if (rest spec)
	    (make-instance 'member-type :objs (rest spec))
	    *the-type-nil*)))
(deftype-symbol-macro null (eql nil))

(defmethod unparse ((type member-type))
  (list* 'member (member-type-objects type)))

;; for some convenience... we'll see if this lasts.
(defclass eql-type (type)
  ((object :initarg :obj :accessor eql-type-object)))

(defmethod typep (object (type eql-type))
  (eql object (eql-type-object type)))

(defmethod subtypep tri/combine ((t1 eql-type) t2)
  "Always certain."
  (values (typep (eql-type-object t1) t2) t))

(defmethod conjoin/2 ((t1 eql-type) (t2 eql-type))
  (if (eql (eql-type-object t1) (eql-type-object t2))
      t1
      *the-type-nil*))

(defmethod disjoin/2 ((t1 eql-type) (t2 eql-type))
  (if (eql (eql-type-object t1) (eql-type-object t2))
      t1
      (make-instance 'member-type
		     :objs (list (eql-type-object t1)
				 (eql-type-object t2)))))

(setf (assoc-value *global-type-environment-specials* 'eql)
      (lambda (spec env)
	(declare (ignore env))
	(destructuring-bind (item) (rest spec)
	  (make-instance 'eql-type :obj item))))

(defmethod unparse ((type eql-type))
  (list 'eql (eql-type-object type)))
