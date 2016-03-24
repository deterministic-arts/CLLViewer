
(in-package "CLL-VIEWER")

(define-command-table global-commands)


(defvar *conditional-commands* nil
  "A list of pairs (`command-name' . `test-function'), which
   are used to conditionally enable/disable commands depending
   on the application status.")


(defclass conditional-command-support () ()
  (:documentation "Mix-in class, which automatically updates
    the command availability of all conditional commands before
    reading an actual command in the command loop."))


(defgeneric frame-update-conditional-commands (frame)
  (:method ((frame conditional-command-support))
    (loop
      :for (command . test) :in *conditional-commands*
      :do (setf (command-enabled command frame) (funcall test frame)))
    frame))


(defun update-conditional-commands (&optional (frame *application-frame*))
  "Evaluate all command tests currently defined, and update the
   command status according to the results in the context of the
   given `frame'."
  (frame-update-conditional-commands frame))


(defmethod read-frame-command :before ((frame conditional-command-support) &rest keys)
  (declare (ignore keys))
  (frame-update-conditional-commands frame))


(defmacro define-command (name-and-options lambda-list &body body)
  (multiple-value-bind (name tester options)
      (if (atom name-and-options)
          (values name-and-options nil nil)
          (let (tester other-options)
            (loop
              :for (key value) :on (cdr name-and-options) :by #'cddr
              :unless (eq key :enabled-if)
                :do (setf other-options (list* value key other-options))
              :else
                :do (setf tester value))
            (values (car name-and-options) tester other-options)))
    `(progn
       ,(if (null tester)
            `(setf *conditional-commands* (remove-if (lambda (pair) (eq (car pair) ',name)) *conditional-commands*))
            `(let ((temp (assoc ',name *conditional-commands*)))
               (if temp
                   (setf (cdr temp) #',tester)
                   (setf *conditional-commands* (cons (cons ',name #',tester) *conditional-commands*)))))
       (clim:define-command (,name ,@(nreverse options)) ,lambda-list ,@body))))



(defparameter +error-key-text-style+ 
  (make-text-style :sans-serif :bold :normal))

(defparameter +error-description-text-style+
  (make-text-style :sans-serif :roman :normal))

(defparameter +error-key-ink+ +red+)


(defun report-general-error (condition &optional (stream *standard-output*))
  (fresh-line stream)
  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-cell (stream :align-x :left :align-y :top)
        (with-text-style (stream +error-key-text-style+)
          (with-drawing-options (stream :ink +error-key-ink+)
            (write-string "Error" stream))))
      (formatting-cell (stream :align-x :left :align-y :top)
        (with-text-style (stream +error-description-text-style+)
          (format stream "~A" condition))))
    (formatting-row (stream)
      (formatting-cell (stream) nil)
      (formatting-cell (stream)
        (with-text-style (stream +error-description-text-style+)
          (format stream "This is a condition of type ~S"
                  (type-of condition)))))))



(defun handle-general-error (condition)
  (let* ((frame *application-frame*)
         (stream (frame-query-io frame))
         (*query-io* stream)
         (*standard-input* *query-io*)
         (*standard-output* *query-io*)
         (*error-output* *query-io*)
         (choices '(member :abort :debug)))
    (report-general-error condition)
    (fresh-line)
    (formatting-table (t)
      (formatting-row (t)
        (formatting-cell (t)
          (with-output-as-presentation (t :abort choices :single-box t)
            (with-text-style (t +error-key-text-style+)
              (write-string "ABORT"))))
        (formatting-cell (t)
          (write-string "Abort the current operation and return to command loop")))
      (formatting-row (t)
        (formatting-cell (t)
          (with-output-as-presentation (t :debug choices :single-box t)
            (with-text-style (t +error-key-text-style+)
              (write-string "DEBUG"))))
        (formatting-cell (t)
          (write-string "Enter the Lisp debugger"))))
    (fresh-line)
    (with-text-style (t +error-key-text-style+)
      (write-string "How to proceed? "))
    (ecase (accept choices :prompt nil)
      ((:abort) (abort))
      ((:debug) (invoke-debugger condition)))))
  

(defmacro with-general-error-handler ((&rest more-handlers) &body body)
  `(handler-bind (,@more-handlers
                  (error #'handle-general-error))
     ,@body))



(defun invoke-centering-output (continuation stream-pane 
                                &key (horizontally t) (vertically t) (hpad 0) (vpad 0))
  (let ((record (with-output-to-output-record (stream-pane)
                  (funcall continuation))))
    (with-bounding-rectangle* (sx0 sy0 sx1 sy1) (sheet-region stream-pane)
      (with-bounding-rectangle* (rx0 ry0 rx1 ry1) (bounding-rectangle record)
        (setf (output-record-position record)
              (values (if horizontally (+ rx0 (/ (- (- sx1 sx0) (- rx1 rx0)) 2)) (+ rx0 hpad))
                      (if vertically (+ ry0 (/ (- (- sy1 sy0) (- ry1 ry0)) 2)) (+ ry0 vpad))))))
    (add-output-record record (stream-output-history stream-pane))
    (repaint-sheet stream-pane record)))

(defmacro centering-output ((&optional (stream 't) &rest options) &body body)
  (let ((stream-var (if (eq stream 't) *standard-output* stream)))
    `(invoke-centering-output (lambda () ,@body) ,stream-var ,@options)))



(defun expand-tabs (line tab-width)
  (let ((stop (position #\tab line)))
    (if (not stop)
        line
        (let ((buffer (make-array (length line) :element-type 'character :fill-pointer 0 :adjustable t))
              (start 0))
          (labels
              ((add-tab ()
                 (let* ((column (length buffer))
                        (next (* tab-width (floor (+ column tab-width) tab-width))))
                   (loop
                     :repeat (- next column)
                     :do (vector-push-extend #\space buffer))))
               (add-range (start end)
                 (loop
                   :for k :upfrom start :below end
                   :do (vector-push-extend (char line k) buffer))))
            (loop
              (when (< start stop) (add-range start stop))
              (add-tab)
              (setf start (1+ stop))
              (let ((next (position #\tab line :start start)))
                (unless next
                  (add-range start (length line)) 
                  (return buffer))
                (setf stop next))))))))

(defun present-plain-text (text stream 
                           &key (start 0) end (tab-width 8))
  (loop
    :for line :in (split-sequence #\newline text 
                                  :start start :end end
                                  :remove-empty-subseqs nil)
    :do (let ((expansion (expand-tabs line tab-width)))
          (write-string expansion stream)
          (terpri stream))))
