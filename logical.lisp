(in-package #:sandalphon.types)

(defclass combination-type (type)
  ((components :initarg :components :accessor combination-type-components))
  (:documentation "The class of types that have an arbitrarily large set of other types as 'components', whatever that means. For now this set is represented as a sequence, but order shouldn't be considered important."))

(defclass conjunction-type (combination-type) ()
  (:documentation "The class of arbitrary conjunctions (e.g. unreduced AND)"))

(defclass disjunction-type (combination-type) ()
  (:documentation "The class of arbitrary disjunctions (e.g. unreduced OR)"))

(defmethod conjoin/2 (t1 t2)
  (cond ((subtypep t1 t2) t1)
	((subtypep t2 t1) t2)
	(t (make-instance 'conjuction-type
			  :components (list t1 t2)))))

(defmethod disjoin/2 (t1 t2)
  (cond ((subtypep t1 t2) t2)
	((subtypep t2 t1) t1)
	(t (make-instance 'disjunction-type
			  :components (list t1 t2)))))

(defmethod typep (obj (type conjunction-type))
  (every (curry #'typep obj) (combination-type-components type)))

(defmethod typep (obj (type disjunction-type))
  (some (curry #'typep obj) (combination-type-components type)))

;;; the thing to remember with these subtypeps
;;;  is that conjunctions are smaller than we can know,
;;;  and disjunctions are bigger
;;;  (when what we know is just the components treated generically)

(defmethod subtypep tri/combine ((t1 conjunction-type) t2)
  ;; &An ⊆ B
  ;; B'  ⊆ (&An)'
  ;; B'  ⊆ |An'
  ;; |(B'⊆ An') ; wrong. {2 4} < {2 3} | {3 4}
  ;; |(An⊆ B)
  ;; 3 applies to more sets than 4, but we can still use 4
  ;;  to give a partial answer.
  ;; (subtypep '(and (member 3 4) (member 4 5)) '(eql 4))
  ;;  => unknown (w/o looking through members)
  (if (some (rcurry #'subtypep t2)
	    (combination-type-components t1))
      ;; some superset of t1 is a definite subtype of t2
      ;; (uncertain or certainly not is irrelevant here)
      (values t t)
      (values nil nil)))

(defmethod subtypep tri/combine (t1 (t2 conjunction-type))
  ;; A ⊆ &Bn
  ;; (&Bn)' ⊆ A'
  ;; (|Bn') ⊆ A'
  ;; |(Bn' ⊆ A') ; fuck. fuck it, fuck this
  (tri/every (curry #'subtypep t1)
	     (combination-type-components t2)))

(defmethod subtypep tri/combine ((t1 disjunction-type) t2)
  (if (tri/notevery (rcurry #'subtypep t2)
		    (combination-type-components t1))
      ;; some subset of t1 is not a subtype of t2
      (values nil t)
      (values nil nil)))

(defmethod subtypep tri/combine (t1 (t2 disjunction-type))
  (if (some (curry #'subtypep t1) (combination-type-components t2))
      ;; t1 is definitely a subtype of some subset of t2
      (values t t)
      (values nil nil)))

(defmethod conjoin/2 (t1 t2)
  (make-instance 'conjunction-type :components (list t1 t2)))

(defmethod disjoin/2 (t1 t2)
  (make-instance 'disjunction-type :components (list t1 t2)))

(defmethod conjoin/2 ((t1 conjunction-type) (t2 conjunction-type))
  (make-instance 'conjunction-type
		 :components (append
			      (combination-type-components t1)
			      (combination-type-components t2))))

(defmethod disjoin/2 ((t1 disjunction-type) (t2 disjunction-type))
  (make-instance 'disjunction-type
		 :components (append
			      (combination-type-components t1)
			      (combination-type-components t2))))

(defmethod-commutative conjoin/2 ((t1 conjunction-type) t2)
  (make-instance
   'conjunction-type
   :components (cons t2 (combination-type-components t1))))
;; these are to force (and (and ...) t/nil) to reduce.
(defmethod conjoin/2 ((t1 conjunction-type) (t2 top-type)) t1)
(defmethod conjoin/2 ((t1 top-type) (t2 conjunction-type)) t2)
(defmethod conjoin/2 ((t1 conjunction-type) (t2 bottom-type)) t2)
(defmethod conjoin/2 ((t1 bottom-type) (t2 conjunction-type)) t1)

(defmethod-commutative disjoin/2 ((t1 disjunction-type) t2)
  (make-instance
   'disjunction-type
   :components (cons t2 (combination-type-components t1))))
;; similarly,
(defmethod disjoin/2 ((t1 disjunction-type) (t2 top-type)) t2)
(defmethod disjoin/2 ((t1 top-type) (t2 disjunction-type)) t1)
(defmethod disjoin/2 ((t1 disjunction-type) (t2 bottom-type)) t1)
(defmethod disjoin/2 ((t1 bottom-type) (t2 disjunction-type)) t2)

(deftype-function and (&rest types) (apply #'conjoin types))
(deftype-function or (&rest types) (apply #'disjoin types))

(defmethod unparse ((type conjunction-type))
  (list* 'and (mapcar #'unparse
		      (combination-type-components type))))
(defmethod unparse ((type disjunction-type))
  (list* 'or (mapcar #'unparse
		     (combination-type-components type))))

(defclass negation-type (type)
  ((negated :initarg :neg :accessor negation-type-type)))

(defmethod typep (object (type negation-type))
  (not (typep object (negation-type-type type))))

(defmethod subtypep tri/combine
    ((t1 negation-type) (t2 negation-type))
  (subtypep (negation-type-type t2) (negation-type-type t1)))

(defmethod negate (type)
  (make-instance 'negation-type :neg type))

(deftype-function not (type)
  (negate type))

(defmethod unparse ((type negation-type))
  (list 'not (unparse (negation-type-type type))))

;;; REAL BOOLEAN ALGEBRA SHIT UP NOW

;;; Basically the priorities are as follows:
;;; 1) Avoid negation-types as much as possible. When they're needed,
;;;    they should be as low in trees as possible.
;;; 2) Try to get a disjunctive normal form: OR of ANDs.

;; De Morgan's laws
(defmethod negate ((type conjunction-type))
  (apply #'disjoin
	 (mapcar #'negate (combination-type-components type))))
(defmethod negate ((type disjunction-type))
  (apply #'conjoin
	 (mapcar #'negate (combination-type-components type))))

;; distribute AND over OR
(defmethod-commutative conjoin/2 ((t1 disjunction-type) t2)
  (apply #'disjoin
	 (mapcar (lambda (type) (conjoin/2 t2 type))
		 (combination-type-components t1))))

;; dunno what this identity's called if anything but it's easy to prove
;; A&~(A|B) = A&~A&B = 0
;; also takes care of A&~A
(defmethod-commutative conjoin/2 ((t1 negation-type) t2)
  (if (subtypep t2 (negation-type-type t1))
      *the-type-nil*
      (call-next-method)))

;; ~A|(A|B) = ~A|A|B = T
;; (or (not (cons integer)) cons)
;; also takes care of A|~A
(defmethod-commutative disjoin/2 ((t1 negation-type) t2)
  (if (subtypep (negation-type-type t1) t2)
      *the-type-t*
      (call-next-method)))
