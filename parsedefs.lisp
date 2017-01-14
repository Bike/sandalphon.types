(in-package #:sandalphon.types)

(deftype-variable t *the-type-t*)

(deftype-variable nil *the-type-nil*)

(macrolet ((def (name et simple)
	     `(deftype-macro ,name (&optional (size '*))
		(list ',(if simple 'simple-array 'array)
		      ',et (list size)))))
  (def simple-vector t t)
  (def base-string base-char nil)
  (def simple-base-string base-char t)
  (def bit-vector bit nil)
  (def simple-bit-vector bit t))

(deftype-macro vector (&optional (element-type '*) (size '*))
  `(array ,element-type (,size)))


(deftype-symbol-macro array (array * *))
(deftype-symbol-macro simple-array (simple-array * *))

(setf (assoc-value *global-type-environment-specials* 'array)
      (lambda (spec env)
	(destructuring-bind
	      (array &optional (element-type '*) (dims '*))
	    spec
	  (declare (ignore array))
	  (make-instance 'array-type
			 :simple nil
			 :element-type
			 (if (eq element-type '*)
			     element-type
			     (parse-type element-type env))
			 :dimensions
			 (etypecase dims
			   ((eql *) dims)
			   ((integer 0)
			    (make-list dims :initial-element '*))
			   (list
			    (loop for dim in dims
			       unless (or (eq dim '*)
					  (and (integerp dim)
					       (plusp dim)))
			       do (error
				   "invalid array dimension ~a"
				   dim))
			    dims))))))

(setf (assoc-value *global-type-environment-specials*
		   'simple-array)
      (lambda (spec env)
	(destructuring-bind
	      (array &optional (element-type '*) (dims '*))
	    spec
	  (declare (ignore array))
	  (make-instance 'array-type
			 :simple t
			 :element-type
			 (if (eq element-type '*)
			     element-type
			     (parse-type element-type env))
			 :dimensions
			 (etypecase dims
			   ((eql *) dims)
			   ((integer 0)
			    (make-list dims :initial-element '*))
			   (list
			    (loop for dim in dims
			       unless (or (eq dim '*)
					  (and (integerp dim)
					       (plusp dim)))
			       do (error
				   "invalid array dimension ~a"
				   dim))
			    dims))))))

(deftype-function cons (&optional car cdr)
  (type-cons (or car *the-type-t*) (or cdr *the-type-t*)))

(deftype-symbol-macro cons (cons t t))
(deftype-symbol-macro atom (not cons))
(deftype-symbol-macro list (or cons null))

;;; ATOMIC TYPE SPECIFIERS (SYMBOL MACROS)

;; make these constant...?

;; these two are impl-dependent (the first indirectly, ofc)
(deftype-symbol-macro bignum (and integer (not fixnum)))
;; required to be a supertype of (signed-byte 16) jsyk
(deftype-symbol-macro fixnum (integer #.most-negative-fixnum
				      #.most-positive-fixnum))

(deftype-symbol-macro bit (integer 0 1))
(deftype-symbol-macro signed-byte integer)
(deftype-symbol-macro unsigned-byte integer)

(deftype-symbol-macro float (float))
(deftype-symbol-macro short-float (short-float))
(deftype-symbol-macro single-float (single-float))
(deftype-symbol-macro double-float (double-float))
(deftype-symbol-macro long-float (long-float))
(deftype-symbol-macro integer (integer))
;; (ratio) is not in CL so we do this. it's eh.
(deftype-variable ratio
    (make-instance 'interval-type
		   :class 'ratio
		   :lower nil :lep nil
		   :upper nil :uep nil))
(deftype-symbol-macro rational (rational))
(deftype-symbol-macro real (real))

;;; COMPOUND TYPE SPECIFIERS

(deftype-macro mod (n) `(integer 0 (,n)))
;; these two copied from SBCL, cos why not.
(deftype-macro signed-byte (&optional (s '*))
  (cond ((eq s '*) 'integer)
        ((and (integerp s) (> s 0))
         (let ((bound (ash 1 (1- s))))
           `(integer ,(- bound) ,(1- bound))))
        (t
         (error
	  "bad size specified for SIGNED-BYTE type specifier: ~S"
	  s))))
(deftype-macro unsigned-byte (&optional (s '*))
  (cond ((eq s '*) '(integer 0))
        ((and (integerp s) (> s 0))
         `(integer 0 ,(1- (ash 1 s))))
        (t
         (error
	  "bad size specified for UNSIGNED-BYTE type specifier: ~S"
	  s))))

(defun parse-interval (spec env)
  (declare (ignore env))
  (destructuring-bind (class &optional lb ub)
      spec
    (check-type class number-class "a class of real numbers")
    (multiple-value-bind (lower lep)
	(cond ((not lb) (values nil nil))
	      ((listp lb) (assert (null (cdr lb)))
	       (values (car lb) t))
	      (t (values lb nil)))
      (multiple-value-bind (upper uep)
	  (cond ((not ub) (values nil nil))
		((listp ub) (assert (null (cdr ub)))
		 (values (car ub) t))
		(t (values ub nil)))
	(assert (or (not lower) (number-in-class-p lower class)))
	(assert (or (not upper) (number-in-class-p upper class)))
	;; FIXME: sanity checks on bounds ((> ub lb) and so on)
	(make-instance 'interval-type
		       :class class
		       :upper upper :uep uep
		       :lower lower :lep lep)))))

(macrolet ((def (name)
	     `(setf (assoc-value *global-type-environment-specials*
				 ',name)
		    'parse-interval))
	   (defs (&rest names)
	     `(progn
		,@(mapcar (lambda (name) `(def ,name)) names))))
  (defs integer rational real) ; (ratio) is not in CL
  (defs float short-float single-float double-float long-float))

(setf (assoc-value *global-type-environment-specials* 'eql)
      (lambda (spec env)
	(declare (ignore env))
	(if (rest spec)
	    (make-instance 'member-type :objs (rest spec))
	    *the-type-nil*)))
(deftype-symbol-macro null (eql nil))

(setf (assoc-value *global-type-environment-specials* 'eql)
      (lambda (spec env)
	(declare (ignore env))
	(destructuring-bind (item) (rest spec)
	  (make-instance 'eql-type :obj item))))

(deftype-function not (type)
  (negate type))

(deftype-function and (&rest types) (apply #'conjoin types))
(deftype-function or (&rest types) (apply #'disjoin types))

(setf (specifier-special 'satisfies nil)
      (lambda (spec env)
	(declare (ignore env))
	(destructuring-bind (predicate) (rest spec)
	  (check-type predicate symbol)
	  (make-instance 'satisfies-type :pred predicate))))
