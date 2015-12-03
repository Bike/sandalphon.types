(in-package #:sandalphon.types)

(defgeneric unparse (type)
  (:documentation "Given a type, return what would be passed to the type parser to produce that type. Mostly just for display."))

(defmethod no-applicable-method ((gf (eql #'unparse)) &rest args)
  ;; avoid an error that when displayed prints the object, since
  ;;  the printer calls UNPARSE, and recursive errors...
  ;;  are bad.
  ;; Error (SIMPLE-ERROR) during printing:
  ;;  #<SIMPLE-ERROR {10040D3283}>
  ;; (for example)
  (destructuring-bind (type) args
    (error "Don't know how to ~a a(n) ~a" 'unparse
	   (class-name (class-of type)))))

(defmethod print-object ((object type) stream)
  (print-unreadable-object (object stream :type t)
    (write (unparse object) :stream stream)))

#+(or)
(defmethod print-object ((object type) stream)
  (print-unreadable-object (object stream :type t)
    (write (or (ignore-errors (unparse object))
	       "No UNPARSE method defined")
	   :stream stream)))
