(in-package #:sandalphon.types)

(defclass cons-type (type)
  ((car :initarg :car :accessor cons-type-car)
   (cdr :initarg :cdr :accessor cons-type-cdr)))

(defmethod typep (obj (type cons-type))
  (and (consp obj)
       (typep (car obj) (cons-type-car type))
       (typep (cdr obj) (cons-type-cdr type))))

(defmethod subtypep tri/combine ((t1 cons-type) (t2 cons-type))
  (tri-and (subtypep (cons-type-car t1) (cons-type-car t2))
	   (subtypep (cons-type-cdr t1) (cons-type-cdr t2))))

(defmethod conjoin/2 ((t1 cons-type) (t2 cons-type))
  (make-instance 'cons-type
		 :car (conjoin/2 (cons-type-car t1)
				 (cons-type-car t2))
		 :cdr (conjoin/2 (cons-type-cdr t1)
				 (cons-type-cdr t2))))
(defmethod disjoin/2 ((t1 cons-type) (t2 cons-type))
  (make-instance 'cons-type
		 :car (disjoin/2 (cons-type-car t1)
				 (cons-type-car t2))
		 :cdr (disjoin/2 (cons-type-cdr t1)
				 (cons-type-cdr t2))))

(deftype-function cons (&optional car cdr)
  (make-instance 'cons-type
		 :car (or car *the-type-t*)
		 :cdr (or cdr *the-type-t*)))

(deftype-symbol-macro cons (cons t t))

(defmethod unparse ((type cons-type))
  (with-slots (car cdr) type
    (let ((car (unparse car))
	  (cdr (unparse cdr)))
      (cond ((not (eq cdr t)) (list 'cons car cdr))
	    ((not (eq car t)) (list 'cons car))
	    (t 'cons)))))
