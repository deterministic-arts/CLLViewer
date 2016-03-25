
(in-package "CLL-UTILS")

(define-condition progress ()
  ((operation
     :initarg :operation :initform nil
     :reader progress-operation)
   (action
     :initarg :action :initform nil
     :reader progress-action)
   (phase
     :initarg :phase :initform nil
     :reader progress-phase)
   (completion
     :type (rational 0 1) :initarg :completion :initform 0
     :reader progress-completion)
   (message
     :type (or null string) :initarg :message :initform nil
     :reader progress-message))
  (:report (lambda (object stream)
             (format stream "~S~@[: ~S~]~@[: ~S~]~@[ (~D % completed)~]~@[~%~A~]"
                     (progress-operation object)
                     (progress-action object)
                     (progress-phase object)
                     (and (plusp (progress-completion object))
                          (floor (* 100 (progress-completion object))))
                     (progress-message object)))))

(defvar *signal-progress* t)
(defvar *progressing-operation* nil)
(defvar *progressing-action* nil)

(defun signal-progress (&key (action *progressing-action*) phase (operation *progressing-operation*) 
                             (completion 0) message)
  (when *signal-progress*
    (signal 'progress
            :action action :operation operation :completion completion
            :message message :phase phase)))
