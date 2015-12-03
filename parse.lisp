(in-package #:sandalphon.types)

(defun parse-type (specifier &optional environment)
  (let ((specifier (typexpand specifier environment)))
    (etypecase specifier
      (symbol (or (specifier-variable specifier environment)
		  (find-class specifier nil environment)
		  (error "~a is not a defined type" specifier)))
      (cons (let ((special
		   (specifier-special (car specifier)
				      environment)))
	      (if special
		  (funcall special specifier environment)
		  (apply (or (specifier-function (car specifier)
						 environment)
			     (error
			      "~a is not a defined type function"
			      (car specifier)))
			 (mapcar (rcurry #'parse-type environment)
				 (rest specifier)))))))))

(defun typexpand-1 (specifier &optional env)
  (let ((expander (etypecase specifier
		    (symbol (specifier-symbol-macro specifier env))
		    (cons (specifier-macro (first specifier)
					   env)))))
    (if expander
	;; could use *macroexpand-hook* later
	(values (funcall expander specifier env) t)
	(values specifier nil))))

(defun typexpand (specifier &optional env)
  (labels ((mux (specifier ever-expanded)
	     (multiple-value-bind (expansion expanded)
		 (typexpand-1 specifier env)
	       (if expanded
		   (mux expansion t)
		   (values specifier ever-expanded)))))
    (mux specifier nil)))

(defvar *global-type-environment-symbol-macros* nil)
(defvar *global-type-environment-macros* nil)
(defvar *global-type-environment-variables* nil)
(defvar *global-type-environment-functions* nil)
(defvar *global-type-environment-specials* nil)

;; this sucks lol. TODO hash tables?
(defmacro remassocf (alist-place key)
  (let ((alist (gensym "ALIST"))
	(gensym (gensym "ASSOC")))
    `(let* ((,alist ,alist-place)
	    (,gensym (assoc ,key ,alist)))
       (when ,gensym
	 (setf ,alist-place (remove ,gensym ,alist))))))

(defgeneric specifier-symbol-macro (spec env)
  (:argument-precedence-order env spec)
  (:method (spec (env null))
    (values
     (assoc-value *global-type-environment-symbol-macros* spec))))
(defgeneric (setf specifier-symbol-macro) (new spec env)
  (:argument-precedence-order env spec new)
  (:method (new spec (env null))
    (check-type new function "a symbol-macro-expander function")
    (if new
	(setf (assoc-value *global-type-environment-symbol-macros*
			   spec)
	      new)
	(remassocf *global-type-environment-symbol-macros* spec))))
(defgeneric specifier-macro (spec env)
  (:argument-precedence-order env spec)
  (:method (spec (env null))
    (values (assoc-value *global-type-environment-macros* spec))))
(defgeneric (setf specifier-macro) (new spec env)
  (:argument-precedence-order env spec new)
  (:method (new spec (env null))
    (check-type new function "a macro-expander function")
    (if new
	(setf (assoc-value *global-type-environment-macros* spec)
	      new)
	(remassocf *global-type-environment-macros* spec))))
(defgeneric specifier-variable (spec env)
  (:argument-precedence-order env spec)
  (:method (spec (env null))
    (values
     (assoc-value *global-type-environment-variables* spec))))
(defgeneric (setf specifier-variable) (new spec env)
  (:argument-precedence-order env spec new)
  (:method (new spec (env null))
    (check-type new type "a type-object")
    (if new
	(setf (assoc-value *global-type-environment-variables*
			   spec)
	      new)
	(remassocf *global-type-environment-variables* spec))))
(defgeneric specifier-function (spec env)
  (:argument-precedence-order env spec)
  (:method (spec (env null))
    (values
     (assoc-value *global-type-environment-functions* spec))))
(defgeneric (setf specifier-function) (new spec env)
  (:argument-precedence-order env spec new)
  (:method (new spec (env null))
    (check-type new function "a type parsing function")
    (if new
	(setf (assoc-value *global-type-environment-functions*
			   spec)
	      new)
	(remassocf *global-type-environment-functions* spec))))
(defgeneric specifier-special (spec env)
  (:argument-precedence-order env spec)
  (:method (spec (env null))
    (values
     (assoc-value *global-type-environment-specials* spec))))
(defgeneric (setf specifier-special) (new spec env)
  (:argument-precedence-order env spec new)
  (:method (new spec (env null))
    (check-type new function "a type special operator function")
    (if new
	(setf (assoc-value *global-type-environment-specials* spec)
	      new)
	(remassocf *global-type-environment-specials* spec))))

(defmacro deftype-variable (name value)
  `(setf (assoc-value *global-type-environment-variables* ',name)
	 ,value))

(defmacro deftype-function (name lambda-list &body body)
  `(setf (assoc-value *global-type-environment-functions* ',name)
	 (lambda ,lambda-list ,@body)))

(defmacro deftype-macro (name lambda-list &body body)
  ;; FIXME: default *, &whole, &environment
  (let ((spec (gensym "SPECIFIER"))
	(env (gensym "ENVIRONMENT")))
    `(setf (assoc-value *global-type-environment-macros* ',name)
	   (lambda (,spec ,env)
	     (declare (ignore ,env))
	     (destructuring-bind ,lambda-list (cdr ,spec)
	       ,@body)))))

(defmacro deftype-symbol-macro (name expansion)
  (let ((spec (gensym "SPECIFIER"))
	(env (gensym "ENVIRONMENT")))
    `(setf (assoc-value *global-type-environment-symbol-macros*
			',name)
	   (lambda (,spec ,env)
	     (declare (ignore ,spec ,env))
	     ',expansion))))
