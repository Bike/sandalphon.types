(in-package #:sandalphon.types)

(defclass cons-type (type)
  ((car :initarg :car :accessor cons-type-car)
   (cdr :initarg :cdr :accessor cons-type-cdr)))

(defmethod typep (obj (type cons-type))
  (and (consp obj)
       (typep (car obj) (cons-type-car type))
       (typep (cdr obj) (cons-type-cdr type))))

(defmethod subtypep tri/combine ((t1 cons-type) (t2 cons-type))
  (tri/and (subtypep (cons-type-car t1) (cons-type-car t2))
	   (subtypep (cons-type-cdr t1) (cons-type-cdr t2))))

(defun type-cons (car cdr)
  "Make a cons type, but can sometimes return NIL, i.e. if either argument is NIL."
  (if (or (cl:typep car 'bottom-type) (cl:typep cdr 'bottom-type))
      *the-type-nil*
      (make-instance 'cons-type :car car :cdr cdr)))

(defmethod conjoin/2 ((t1 cons-type) (t2 cons-type))
  (type-cons (conjoin/2 (cons-type-car t1) (cons-type-car t2))
	     (conjoin/2 (cons-type-cdr t1) (cons-type-cdr t2))))
(defmethod disjoin/2 ((t1 cons-type) (t2 cons-type))
  ;; can't really get nil here, but might as well be consistent.
  (type-cons (disjoin/2 (cons-type-car t1) (cons-type-car t2))
	     (disjoin/2 (cons-type-cdr t1) (cons-type-cdr t2))))

(defmethod negate ((type cons-type))
  (if (and (eq (cons-type-car type) *the-type-t*)
	   (eq (cons-type-cdr type) *the-type-t*))
      (call-next-method) ; avoid recurse
      (disjoin (negate (type-cons *the-type-t* *the-type-t*))
	       (type-cons (negate (cons-type-car type))
			  (cons-type-cdr type))
	       (type-cons (cons-type-car type)
			  (negate (cons-type-cdr type))))))

(deftype-function cons (&optional car cdr)
  (type-cons (or car *the-type-t*) (or cdr *the-type-t*)))

(deftype-symbol-macro cons (cons t t))
(deftype-symbol-macro atom (not cons))
(deftype-symbol-macro list (or cons null))

(defmethod unparse ((type cons-type))
  (with-slots (car cdr) type
    (let ((car (unparse car))
	  (cdr (unparse cdr)))
      (cond ((not (eq cdr t)) (list 'cons car cdr))
	    ((not (eq car t)) (list 'cons car))
	    (t 'cons)))))
