
(in-package #:cll-model-internals)

(defclass basic-store (db:connection store)
  ((text-file
     :type pathname :initarg :text-file
     :reader store-text-file)))

(defclass basic-transaction (db:transaction transaction)
  ())

(defun open-store (object &key)
  (let* ((db-base-name (load-time-value (make-pathname :type "db" :version :newest)))
         (text-base-name (load-time-value (make-pathname :type "txt" :version :newest)))
         (db-pathname (merge-pathnames db-base-name object))
         (text-pathname (merge-pathnames text-base-name object)))
    (db:open-connection db-pathname
                        :class 'basic-store
                        :text-file text-pathname)))

(defun close-store (object)
  (db:close-connection object))

(defmethod db:connection-transaction-class ((object basic-store))
  (load-time-value (find-class 'basic-transaction)))

(defmacro with-store ((var pathname &rest options) &body body)
  `(invoke-with-store (lambda (,var) ,@body) ,pathname ,@options))

(defun invoke-with-store (function pathname &rest options &key &allow-other-keys)
  (let ((store (apply #'open-store pathname options)))
    (unwind-protect (funcall function store)
      (close-store store))))
