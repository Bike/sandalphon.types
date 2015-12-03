(in-package #:sandalphon.types)

(defmacro tri/combine (&rest forms)
  "Evaluate FORMS from left to right until one is certain, and return that."
  (cond ((endp forms) (values nil nil)) ; whatever
	;; (the t ...) is to force nontoplevelness.
	((endp (rest forms)) `(the t ,(first forms)))
	(t (with-gensyms (result certainty)
	     `(multiple-value-bind (,result ,certainty) ,(first forms)
		(if ,certainty
		    (values ,result ,certainty)
		    (tri/combine ,@(rest forms))))))))

(defun tri/every (pred first-seq &rest more-seqs)
  "Like CL:EVERY but with trivalent values."
  (let ((overall-certainty t))
    (flet ((map-me (&rest rest)
	     (multiple-value-bind (value certainty)
		 (apply pred rest)
	       (unless value
		 (return-from tri/every (values nil t)))
	       (unless certainty (setf overall-certainty nil)))))
      (apply #'map nil #'map-me first-seq more-seqs)
      (if overall-certainty
	  (values t t)
	  (values nil nil)))))

(defun tri/notany (pred first-seq &rest more-seqs)
  "Like CL:NOTANY, but with trivalent values."
  (let ((overall-certainty t))
    (flet ((map-me (&rest rest)
	     (multiple-value-bind (value certainty)
		 (apply pred rest)
	       (unless certainty (setf overall-certainty nil))
	       (when value
		 (return-from tri/notany (values nil t))))))
      (declare (inline map-me))
      (apply #'map nil #'map-me first-seq more-seqs)
      (if overall-certainty
	  (values t t)
	  (values nil nil)))))

(defun tri/notevery (pred first-seq &rest more-seqs)
  "Like CL:NOTEVERY, but with trivalent values."
  (let ((overall-certainty t))
    (flet ((map-me (&rest rest)
	     (multiple-value-bind (value certainty)
		 (apply pred rest)
	       (if certainty
		   (unless value
		     (return-from tri/notevery (values t t)))
		   (setf overall/certainty nil)))))
      (declare (inline map-me))
      (apply #'map nil #'map-me first-seq more-seqs)
      (values nil overall-certainty))))

(defmacro tri/not (form)
  (with-gensyms (result certainty)
    `(multiple-value-bind (,result ,certainty) ,form
       (if ,certainty
	   (values (not ,result) ,certainty)
	   (values nil nil)))))

(defmacro tri/or (&rest forms)
  ;; true | unknown = true, false | unknown = unknown
  (cond ((endp forms) (values nil nil))
	((endp (rest forms))
	 `(the t ,(first forms)))
	(t (let ((result (gensym "RESULT"))
		 (certainty (gensym "CERTAINTY")))
	     `(multiple-value-bind (,result ,certainty)
		  ,(first forms)
		(if ,result
		    (values ,result t)
		    (tri/or ,@(rest forms))))))))

(defmacro tri/and (&rest forms)
  ;; true & unknown = unknown, false & unknown = false
  (cond ((endp forms) (values nil nil))
	((endp (rest forms))
	 `(the t ,(first forms)))
	(t (let ((result (gensym "RESULT"))
		 (certainty (gensym "CERTAINTY")))
	     `(multiple-value-bind (,result ,certainty)
		  ,(first forms)
		(if (and ,certainty (not ,result)) ; false
		    (values ,certainty ,result)
		    (tri/and ,@(rest forms))))))))
