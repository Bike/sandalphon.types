(in-package #:sandalphon.types)
;;;; CL function types are different from other types in that they
;;;; "can be used only for declaration and not for discrimination".
;;;; This means that typep and subtypep are meaningless on them.
;;;; How they're ACTUALLY used is to restrict calls:
;;;; (defun foo (fn)
;;;;   (declare (type (function (float string) list) fn))
;;;;   ... (funcall fn a b) ...)
;;;; Here the compiler can, e.g., assume that a is a float,
;;;;  even if fn happens to be #'cons at runtime.
;;;; So we try to support that a bit, at least.
;;;; Actual types-of-functions are much broader and will be a
;;;;  a separate, CL-extending thing.

(defclass function-type (type)
  ((ll-spec :initarg :ll-spec
	    :accessor function-type-lambda-list-specified-p)
   (reqs :initarg :reqs :accessor function-type-reqs)
   (opts :initarg :opts :accessor function-type-opts)
   (rest :initarg :rest :accessor function-type-rest)
   (keynames :initarg :keyns :accessor function-type-key-names)
   (keytypes :initarg :keyts :accessor function-type-key-types)
   (aok-p :initarg :aok-p :accessor function-type-aok-p)
   (result :initarg :result :accessor function-type-result)))

(defun parse-function-type (spec env)
  (flet ((parse (spec) (parse-type spec env)))
    (destructuring-bind (&optional lambda-list return-type)
	(rest spec)
      (when (eql lambda-list '*) (setf lambda-list nil))
      (when (eql return-type '*) (setf return-type nil))
      (multiple-value-bind (reqs opts rest keyns keyts aok-p)
	  (if lambda-list
	      (multiple-value-bind
		    (reqs opts rest restp keyns keyts aok-p)
		  (parse-funtype-lambda-list lambda-list)
		(values (mapcar #'parse reqs) (mapcar #'parse opts)
			(when restp (parse rest))
			keyns (mapcar #'parse keyts)
			aok-p))
	      (values nil nil nil nil nil nil))
	(make-instance 'function-type
		       :ll-spec (if lambda-list t nil)
		       :reqs reqs :opts opts :rest rest
		       :keyns keyns :keyts keyts :aok-p aok-p
		       :result (and return-type
				    (parse return-type)))))))

(setf (specifier-special 'function nil) #'parse-function-type)

(defmethod unparse ((type function-type))
  (with-slots (ll-spec reqs opts rest
		       keynames keytypes aok-p result)
      type
    (let ((ll (if ll-spec
		  (append (mapcar #'unparse reqs)
			  (when opts '(&optional))
			  (mapcar #'unparse opts)
			  (when rest (list '&rest (unparse rest)))
			  (when keynames '(&key))
			  (mapcar #'list keynames
				  (mapcar #'unparse keytypes))
			  (when aok-p '(&allow-other-keys)))
		  '*)))
      (cond (result (list 'function ll (unparse result)))
	    ;; printers are sometimes overzealous with just
	    ;;  (function foo) so we keep an asterisk.
	    ((not (eq ll '*)) (list 'function ll '*))
	    (t 'function)))))

(defun type-of-call (function args)
  "Returns (values RESULT-TYPE ARGS-TYPE VALID-P CERTAIN-P).
FUNCTION is a function type. (This will be extended later.)
ARGS is the type of the arguments it is called with, as by APPLY.
RESULT-TYPE is type of the call form.
ARGS-TYPE is ARGS, but constrained as much as possible.
VALID-P and CERTAIN-P are trivalent, expressing whether the call is legal (i.e. types conform).

Experimental, future expansion, etc."
  (with-slots (ll-spec reqs opts rest
		       keynames keytypes aok-p result)
      function
    (let ((base-return (or result *the-type-t*)))
      (if (not ll-spec)
	  (values args base-return t t)
	  ;; i should do something else for now.
	  (values args base-return nil nil)))))

(defun parse-funtype-lambda-list (lambda-list)
  "Parses a lambda list valid to a FUNCTION type specifier.
Returns (values required optional rest restp key-names key-types aok-p).
restp is necessary because &rest nil is okay... I guess."
  ;; basically a more forgiving, and less flexible,
  ;;  alexandria:parse-ordinary-lambda-list
  (let ((state :required)
	(required nil)
	(optional nil)
	(rest nil)
	(restp nil)
	(key-keys nil)
	(key-types nil)
	(aok-p nil))
    (flet ((state-transition (from arg)
	     (if (find state from)
		 (setf state arg)
		 (simple-program-error "Misplaced ~s in ~a type-specifier lambda-list:~%  ~s" arg 'function lambda-list))))
      (dolist (elt lambda-list)
	(case elt
	  ((&optional) (state-transition '(:required) elt))
	  ((&rest) (state-transition '(:required &optional) elt)
	   (setf restp t))
	  ((&key)
	   (state-transition '(:required &optional :after-rest)
			     elt))
	  ((&allow-other-keys) (state-transition '(&key) elt)
	   (setf aok-p t))
	  (t
	   (ecase state
	     ;; could err on binding constants, but like, shrug.
	     ((:required) (push elt required))
	     ((&optional) (push elt optional))
	     ((&rest) (setf rest elt state :after-rest))
	     ((:after-rest)
	      (simple-program-error "~s is after a ~a parameter, and so should be a lambda-list keyword, in ~a type-specifier lambda-list:~%  ~s" elt '&rest 'function lambda-list))
	     ((&key)
	      (unless (and (consp elt)
			   (consp (cdr elt))
			   (null (cddr elt))
			   (symbolp (first elt)))
		(simple-program-error "~a is not a valid ~s parameter in a ~a type-specifier lambda-list~%  ~s~%  The correct form is (symbol type)." elt '&key 'function lambda-list))
	      (push (first elt) key-keys)
	      (push (second elt) key-types))
	     ((&allow-other-keys)
	      (simple-program-error "Misplaced type ~s after ~a in ~a type-specifier lambda-list:~%  ~s" elt '&allow-other-keys 'function lambda-list)))))))
    (when (eq state '&rest)
      (simple-program-error "Forgot a variable after ~a in ~a type-specifier lambda-list:~%  ~s" '&rest 'function lambda-list))
    (values (nreverse required) (nreverse optional) rest restp
	    (nreverse key-keys) (nreverse key-types) aok-p)))
