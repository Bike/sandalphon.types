(in-package #:sandalphon.types)

(defmethod unparse ((type class)) (class-name type))

(defmethod unparse ((type top-type)) 't)
(defmethod unparse ((type bottom-type)) 'nil)

(defmethod unparse ((type array-type))
  ;; TODO: beef up (simple-bit-array, etc)
  (with-slots (element-type dimensions simplicity)
      type
    (let ((et (if (eq element-type '*)
		  '*
		  (unparse element-type)))
	  (base (if simplicity 'simple-array 'array)))
      (cond ((not (eq dimensions '*))
	     (list base et dimensions))
	    ((not (eq et '*))
	     (list base et))
	    (t base)))))

(defmethod unparse ((type cons-type))
  (with-slots (car cdr) type
    (let ((car (unparse car))
	  (cdr (unparse cdr)))
      (cond ((not (eq cdr t)) (list 'cons car cdr))
	    ((not (eq car t)) (list 'cons car))
	    (t 'cons)))))

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

(defmethod unparse ((type eql-type))
  (list 'eql (eql-type-object type)))

(defmethod unparse ((type member-type))
  (list* 'member (member-type-objects type)))

(defmethod unparse ((type negation-type))
  (list 'not (unparse (negation-type-type type))))

(defmethod unparse ((type conjunction-type))
  (list* 'and (mapcar #'unparse
		      (combination-type-components type))))
(defmethod unparse ((type disjunction-type))
  (list* 'or (mapcar #'unparse
		     (combination-type-components type))))

(defmethod unparse ((type satisfies-type))
  (list 'satisfies (satisfies-type-predicate-name type)))
