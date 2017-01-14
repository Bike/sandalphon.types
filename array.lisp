(in-package #:sandalphon.types)

(defclass array-type (type)
  (;; upgraded
   (element-type :initarg :element-type
		 :accessor array-type-element-type)
   ;; normalized to a list
   (dimensions :initarg :dimensions
	       :accessor array-type-dimensions)
   (simplicity :initarg :simple :accessor array-type-simple-p)))

(defun array-type-dimension-< (d1 d2)
  (or (eq d2 '*)
      (and (not (eq d2 '*) d1) (= d1 d2))))

(defmethod typep (obj (type array-type))
  (and (if (array-type-simple-p type)
	   ;; this is implementation dependent per
	   ;;  clhs simple-array.
	   ;; the following conditions make our simple-array,
	   ;;  at worst, a strict subset of the implementation's.
	   (not (or (array-has-fill-pointer-p obj)
		    (array-displacement obj)
		    (adjustable-array-p obj)))
	   t)
       (if (eq (array-type-dimensions type) '*)
	   t ; any dims are ok
	   (and (= (array-rank obj)
		   (length (array-type-dimensions type)))
		(every #'array-type-dimension-<
		       (array-dimensions obj)
		       (array-type-dimensions type))))
       (if (eq (array-type-element-type type) '*)
	   t
	   ;; use of parse-type this early is gross but apparently
	   ;;  necessary.
	   ;; type= is trivalent, so that's more gross,
	   ;; but probably won't come up much in practice
	   (values (type= (parse-type (array-element-type type))
			  (array-type-element-type type))))))

(defmethod subtypep tri/combine ((t1 array-type) (t2 array-type))
  (when (and (array-type-simple-p t2)
	     (not (array-type-simple-p t1)))
    (return-from subtypep (values nil t)))
  (unless (eq (array-type-dimensions t2) '*)
    (when (eq (array-type-dimensions t1) '*)
      (return-from subtypep (values nil t)))
    (unless (= (length (array-type-dimensions t1))
	       (length (array-type-dimensions t2)))
      (return-from subtypep (values nil t)))
    (unless (every #'array-type-dimension-<
		   (array-type-dimensions t1)
		   (array-type-dimensions t2))
      (return-from subtypep (values nil t))))
  (cond ((eq (array-type-element-type t2) '*)
	 (values t t))
	((eq (array-type-element-type t1) '*)
	 (values nil t))
	(t (type= (array-type-element-type t1)
		  (array-type-element-type t2)))))

(defmethod conjoin/2 ((t1 array-type) (t2 array-type))
  (multiple-value-bind (result certainty)
      (type= (array-type-element-type t1) (array-type-element-type t2))
    (cond (result
	   (let ((dims
		  (cond ((eq (array-type-dimensions t2) '*)
			 (array-type-dimensions t1))
			((eq (array-type-dimensions t1) '*)
			 (array-type-dimensions t2))
			(t
			 (if (= (length (array-type-dimensions t1))
				(length
				 (array-type-dimensions t2)))
			     (mapcar
			      (lambda (d1 d2)
				(cond ((eq d1 '*) d2)
				      ((eq d2 '*) d1)
				      ((= d1 d2) d1)
				      (t
				       (return-from conjoin/2
					 *the-type-nil*))))
			      (array-type-dimensions t1)
			      (array-type-dimensions t2))
			     (return-from conjoin/2
			       *the-type-nil*))))))
	     (make-instance 'array-type
			    :element-type
			    (array-type-element-type t1)
			    :dimensions dims
			    :simple (and
				     (array-type-simple-p t1)
				     (array-type-simple-p t2)))))
	  (certainty *the-type-nil*)
	  ;; I think the best thing to do on uncertainty on the type= is punt.
	  (t (call-next-method)))))

(defmethod disjoin/2 ((t1 array-type) (t2 array-type))
  (multiple-value-bind (result certainty)
      (type= (array-type-element-type t1)
	     (array-type-element-type t2))
    (if result
	(let ((dims
	       (cond ((eq (array-type-dimensions t2) '*) '*)
		     ((eq (array-type-dimensions t1) '*) '*)
		     (t (if (= (length (array-type-dimensions t1))
			       (length (array-type-dimensions t2)))
			    (mapcar (lambda (d1 d2)
				      (cond
					((eq d1 '*) d1)
					((eq d2 '*) d2)
					((= d1 d2) d1)
					(t
					 (return-from disjoin/2
					   (call-next-method)))))
				    (array-type-dimensions t1)
				    (array-type-dimensions t2))
			    (return-from disjoin/2
			      (call-next-method)))))))
	  (make-instance 'array-type
			 :element-type (array-type-element-type t1)
			 :dimensions dims
			 :simple (or (array-type-simple-p t1)
				     (array-type-simple-p t2))))
	(call-next-method))))
