(in-package #:sandalphon.types)

(defclass interval-type (type)
  ((class :initarg :class
		 :accessor interval-type-number-class
		 :documentation "The number's \"class\", e.g. INTEGER. See NUMBER-IN-CLASS-P, NUMBER-CLASS-SUBTYPEP.")
   (upper :initarg :upper :accessor interval-type-upper
	  :documentation "Upper bound. NIL indicates infinity.")
   (upper-exclusive-p :initarg :uep
		      :accessor interval-type-upper-exclusive-p)
   (lower :initarg :lower :accessor interval-type-lower
	  :documentation "Lower bound. NIL indicates -infinity.")
   (lower-exclusive-p :initarg :lep
		      :accessor interval-type-lower-exclusive-p)))

;; internal
(cl:deftype number-class ()
  '(member real rational integer ratio float
    short-float double-float long-float))

(defun number-in-class-p (obj class)
  ;; would like to avoid typep as much as possible.
  ;;  it's a huge function (e.g. this library)
  ;; hopefully typep on constant type
  ;;  is at least reasonably efficient.
  (ecase class
    ((real) (realp obj))
    ((rational) (rationalp obj))
    ((integer) (integerp obj))
    ((ratio) (cl:typep obj 'ratio))
    ((float) (floatp obj))
    ((short-float) (cl:typep obj 'short-float))
    ((single-float) (cl:typep obj 'single-float))
    ((double-float) (cl:typep obj 'double-float))
    ((long-float) (cl:typep obj 'long-float))))

(defun number-class-subtypep (c1 c2)
  (ecase c2
    ((real) t)
    ((rational) (or (eq c1 'rational)
		    (eq c1 'integer) (eq c1 'ratio)))
    ((integer) (eq c1 'integer))
    ((ratio) (eq c1 'ratio))
    ((float) (or (eq c1 'float) (eq c1 'short-float)
		 (eq c1 'single-float) (eq c1 'double-float)
		 (eq c1 'long-float)))
    ((short-float single-float double-float long-float)
     ;; IMPLEMENTATION DEFINED: relations between float subtypes
     ;; Note that conformingly, this always returns a certainty.
     (values (cl:subtypep c1 c2)))))

(defmethod typep (obj (type interval-type))
  (with-slots (class upper upper-exclusive-p
		     lower lower-exclusive-p)
      type
    (and (number-in-class-p obj class)
	 (or (not upper)
	     (< obj upper)
	     (and (not upper-exclusive-p)
		  (= obj upper)))
	 (or (not lower)
	     (> obj lower)
	     (and (not lower-exclusive-p)
		  (= obj lower))))))

(defun ub<ub (u1 uep1 u2 uep2)
  (or (not u2) ; if 2 has no ub, obviously (< u1 u2)
      (and u1 ; (> infinity anything)
	   (or (< u1 u2)
	       (and (= u1 u2)
		    ;; if uep2, uep1 should be too
		    ;; if ~uep2 doesn't matter
		    (or (not uep2) uep1))))))

(defun lb>lb (l1 lep1 l2 lep2)
  (or (not l2)
      (and l1
	   (or (> l1 l2)
	       (and (= l1 l2)
		    (or (not lep2) lep1))))))

(defmethod subtypep tri/combine
    ((t1 interval-type) (t2 interval-type))
  "Always certain."
  (with-slots ((class1 class) (u1 upper) (uep1 upper-exclusive-p)
	       (l1 lower) (lep1 lower-exclusive-p))
      t1
    (with-slots ((class2 class) (u2 upper) (uep2 upper-exclusive-p)
		 (l2 lower) (lep2 lower-exclusive-p))
	t2
      (values
       (and (number-class-subtypep class1 class2)
	    (ub<ub u1 uep1 u2 uep2)
	    (lb>lb l1 lep1 l2 lep2))
       t))))

(defun least-upper-bound (u1 uep1 u2 uep2)
  (cond ((not u2) (values u1 uep1))
	((not u1) (values u2 uep2))
	((< u1 u2) (values u1 uep1))
	((< u2 u1) (values u2 uep2))
	((= u1 u2) (values u1 (or uep1 uep2)))))

(defun greatest-lower-bound (l1 lep1 l2 lep2)
  (cond ((not l2) (values l1 lep1))
	((not l1) (values l2 lep2))
	((> l1 l2) (values l1 lep1))
	((> l2 l1) (values l2 lep2))
	((= l1 l2) (values l1 (or lep1 lep2)))))

(defmethod conjoin/2 ((t1 interval-type) (t2 interval-type))
  (with-slots ((class1 class) (u1 upper) (uep1 upper-exclusive-p)
	       (l1 lower) (lep1 lower-exclusive-p))
      t1
    (with-slots ((class2 class) (u2 upper) (uep2 upper-exclusive-p)
		 (l2 lower) (lep2 lower-exclusive-p))
	t2
      (flet ((ret (class)
	       (multiple-value-bind (newu newuep)
		   (least-upper-bound u1 uep1 u2 uep2)
		 (multiple-value-bind (newl newlep)
		     (greatest-lower-bound l1 lep1 l2 lep2)
		   ;; make sure our intersection isn't disjoint.
		   (cond ((and newl newu (> newl newu)) *the-type-nil*)
			 ((and newl newu (= newl newu))
			  (if (or newuep newlep)
			      *the-type-nil*
			      (make-instance 'eql-type :obj newl)))
			 (t (make-instance
			     'interval-type
			     :class class
			     :upper newu :uep newuep
			     :lower newl :lep newlep)))))))
	(cond ((number-class-subtypep class1 class2)
	       (ret class1))
	      ((number-class-subtypep class2 class1)
	       (ret class2))
	      (t *the-type-nil*))))))

(defun lb<=ub (l1 lep1 u2 uep2)
  (or (not l1)
      (not u2)
      (< l1 u2)
      (and (= l1 u2)
	   (not (and lep1 uep2)))))

(defun greatest-upper-bound (u1 uep1 u2 uep2)
  (cond ((not u1) (values u1 uep1))
	((not u2) (values u2 uep2))
	((> u1 u2) (values u1 uep1))
	((> u2 u1) (values u2 uep2))
	((= u1 u2) (values u1 (and uep1 uep2)))))

(defun least-lower-bound (l1 lep1 l2 lep2)
  (cond ((not l1) (values l1 lep1))
	((not l2) (values l2 lep2))
	((< l1 l2) (values l1 lep1))
	((< l2 l1) (values l2 lep2))
	((= l1 l2) (values l1 (and lep1 lep2)))))

;; i thought this would be more involved than conjoin,
;;  but it's not, so i probably fucked something up.
(defmethod disjoin/2 ((t1 interval-type) (t2 interval-type))
  (with-slots ((class1 class) (u1 upper) (uep1 upper-exclusive-p)
	       (l1 lower) (lep1 lower-exclusive-p))
      t1
    (with-slots ((class2 class) (u2 upper) (uep2 upper-exclusive-p)
		 (l2 lower) (lep2 lower-exclusive-p))
	t2
      (if (and (eq class1 class2)
	       ;; ensure there is overlap
	       (and (lb<=ub l1 lep1 u2 uep2)
		    (lb<=ub l2 lep2 u1 uep1)))
	  (multiple-value-bind (newu newuep)
	      (greatest-upper-bound u1 uep1 u2 uep2)
	    (multiple-value-bind (newl newlep)
		(least-lower-bound l1 lep1 l2 lep2)
	      (make-instance 'interval-type
			     :class class1
			     :upper newu :uep newuep
			     :lower newl :lep newlep)))
	  (call-next-method)))))

(defmethod negate ((type interval-type))
  ;; in general this will be (or (not class) interval interval).
  ;; e.g., (not (integer -10 10))
  ;;  => (or (not integer) (integer * (-10)) (integer (10) *))
  ;; if one of the bounds is not a bound, of course,
  ;;  the upper and lower intervals can be nothing.
  (with-slots (class upper (uep upper-exclusive-p)
		     lower (lep lower-exclusive-p))
      type
    (let ((uptype (if upper
		      (make-instance 'interval-type
				     :class class
				     :lower upper :lep (not uep)
				     :upper nil :uep nil)
		      *the-type-nil*))
	  (dntype (if lower
		      (make-instance 'interval-type
				     :class class
				     :lower nil :lep nil
				     :upper lower :uep (not lep))
		      *the-type-nil*)))
      (if (and (not upper) (not lower))
	  ;; (not integer); avoid infinite regress by punting
	  (call-next-method)
	  ;; we could also make a negation type directly,
	  ;;  but then we need to depend on logical
	  ;;  so why bother.
	  (disjoin (negate (make-instance 'interval-type
					  :class class
					  :lower nil :lep nil
					  :upper nil :uep nil))
		   uptype dntype)))))

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

;;; UNPARSING

(defmethod unparse ((type interval-type))
  ;; TODO: get swole
  (with-slots (class upper upper-exclusive-p
		     lower lower-exclusive-p)
      type
    ;; this assumes that lep/uep will be NIL if upper/lower is.
    (let ((lb (cond (lower-exclusive-p (list lower))
		    (lower lower)
		    ; infinite
		    (t '*)))
	  (ub (cond (upper-exclusive-p (list upper))
		    (upper upper)
		    (t '*))))
      (cond (upper (list class lb ub))
	    (lower (list class lb))
	    (t class)))))
