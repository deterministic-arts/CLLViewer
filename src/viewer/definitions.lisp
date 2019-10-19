
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
      (formatting-cell (stream) (declare (ignore stream)))
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



(defun draw-progress-bar (stream x y width height ratio
                          &key (border-ink +foreground-ink+) (bar-ink +foreground-ink+)
                               (background-ink +background-ink+))
  (draw-rectangle* stream x y (+ x (1- width)) (+ y (1- height)) :filled nil :ink border-ink)
  (draw-rectangle* stream (1+ x) (1+ y) (+ x (1- width)) (+ y (1- height)) :filled t :ink background-ink)
  (when (plusp ratio)
    (let* ((inner-width (- width 2))
           (inner-height (- height 2))
           (length (floor (* inner-width (max 0 (min 1 ratio))))))
      (draw-rectangle* stream (+ x 2) (+ y 2) (+ x length) (+ y inner-height)
                       :filled t :ink bar-ink)))
  stream)

(defclass progress-bar-view (view)
  ((width
     :initarg :width :initform 200
     :reader progress-bar-view-width)
   (border-ink
     :initarg :border-ink :initform +foreground-ink+
     :reader progress-bar-view-border-ink)
   (background-ink
     :initarg :background-ink :initform +background-ink+
     :reader progress-bar-view-background-ink)
   (bar-ink
     :initarg :bar-ink :initform +foreground-ink+
     :reader progress-bar-view-bar-ink)))

(defparameter +progress-bar-view+ (make-instance 'progress-bar-view))
(defparameter +light-progress-bar-view+ (make-instance 'progress-bar-view :border-ink +gray40+ :bar-ink +gray60+))

(define-presentation-type progress-value ()
  :inherit-from '(real 0 1))

(define-presentation-method present (object (type progress-value) stream (view progress-bar-view) &key acceptably)
  (if acceptably
      (call-next-method)
      (with-sheet-medium (medium stream)
        (let* ((width (progress-bar-view-width view))
               (style (medium-text-style medium))
               (ascent (text-style-ascent style medium)))
          (multiple-value-bind (cx cy) (stream-cursor-position stream)
            (draw-progress-bar stream cx cy width ascent object
                               :border-ink (progress-bar-view-border-ink view)
                               :background-ink (progress-bar-view-background-ink view)
                               :bar-ink (progress-bar-view-bar-ink view))
            (setf (stream-cursor-position stream) (values (+ cx width) cy)))))))

(defun invoke-with-inline-progress (function items
                                    &key (stream *standard-output*) sum-label
                                         (partial-view +progress-bar-view+)
                                         (total-view +progress-bar-view+))
  (labels
      ((stringify (symbol)
         (if (stringp symbol) symbol
             (string-capitalize (substitute #\space #\- (symbol-name symbol)))))
       (parse-item (item)
         (destructuring-bind (name &key (label (stringify name))) (if (consp item) item (list item))
           (values name label))))
    (let* (names progress labels (count 0) (total 0) (total-key (gensym)))
      (loop
         for elt in (reverse items)
         do (multiple-value-bind (name label) (parse-item elt)
              (push name names)
              (push 0 progress)
              (push label labels)
              (incf count)))
      (setf sum-label (and sum-label (stringify sum-label)))
      (let ((record (updating-output (stream)
                      (mapcar (lambda (name progress label)
                                (fresh-line stream)
                                (stream-increment-cursor-position stream 2 0)
                                (updating-output (stream :unique-id name :cache-value progress)
                                  (present progress 'progress-value :stream stream :view partial-view)
                                  (format stream " ~A (~,1F%)" label (* 100 progress))))
                              names progress labels)
                      (when sum-label
                        (with-text-face (stream :bold)
                        (fresh-line stream)
                        (stream-increment-cursor-position stream 2 0)
                        (updating-output (stream :unique-id total-key :cache-value total)
                          (present total 'progress-value :stream stream :view total-view)
                          (format stream " ~A (~,1F%)" sum-label (* 100 total))))))))
        (funcall function (lambda (&rest pairs)
                            (let (changed)
                              (loop
                                 for (key value) on pairs by #'cddr
                                 do (loop
                                       for n in names
                                       for p on progress
                                       when (eql n key)
                                       do (setf (car p) value)
                                          (setf changed t)
                                          (return)))
                              (when changed
                                (when sum-label
                                  (setf total (/ (reduce #'+ progress :initial-value 0) count)))
                                (redisplay record stream)))))))))

(defmacro with-inline-progress ((report &optional (stream 't) &rest options) items &body body)
  (setf stream (if (eq stream 't) '*standard-output* stream))
  (let ((reporter (gensym)))
    `(invoke-with-inline-progress (lambda (,reporter)
                                    (flet ((,report (key value) (funcall ,reporter key value)))
                                      ,@body))
                                  ,items
                                  :stream ,stream ,@options)))
