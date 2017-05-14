
(in-package "CLL-VIEWER")

(defclass callback-event (device-event)
  ((function :initarg :function :reader callback-event-function)
   (arguments :initform nil :initarg :arguments :reader callback-event-arguments))
  (:default-initargs :modifier-state 0))

(defun invoke-in-frame-event-loop (function frame &key arguments)
  (let* ((sheet (frame-top-level-sheet frame))
         (event (make-instance 'callback-event :function function :arguments arguments :sheet frame)))
    (queue-event sheet event)
    frame))

(defmethod handle-event ((frame application-frame) (event callback-event))
  (apply (callback-event-function event)
         (callback-event-arguments event)))

(defmethod handle-event ((frame pane) (event callback-event))
  (apply (callback-event-function event)
         (callback-event-arguments event)))



(define-application-frame progress-view ()
  ((operation :initarg :operation :initform nil :accessor progress-view-operation)
   (message :initarg :message :initform nil :accessor progress-view-message)
   (completion :initarg :completion :initform 0 :accessor progress-view-completion)
   (bar-length :initform 0 :accessor progress-bar-length))
  (:panes
    (title :application
      :height '(2 :line) :min-height '(2 :line) :max-height '(2 :line)
      :name 'title
      :borders nil
      :background +white+
      :display-function 'display-progress-title
      :scroll-bars nil
      :display-time t
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (message :application
      :height '(3 :line) :min-height '(3 :line) :max-height '(3 :line)
      :name 'message
      :borders nil
      :background +white+
      :display-function 'display-progress-message
      :scroll-bars nil
      :display-time t
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (completion :application 
      :height '(2 :line) :min-height '(2 :line) :max-height '(2 :line)
      :name 'completion
      :borders nil
      :background +gray40+ 
      :display-function 'display-progress-completion
      :scroll-bars nil
      :display-time t
      :end-of-line-action :allow
      :end-of-page-action :allow))
  (:layouts 
    (default 
      (spacing (:thickness 3 :background +white+)
        (vertically (:y-spacing 3 :background +white+)
          title 
          message
          completion)))))


#-(and)
(defmethod handle-event ((object progress-view) (event progress-event))
  (format *terminal-io* "~&Handle-Event(~S, ~S)~%" object event)
  (funcall (progress-event-action event)))


(defun display-progress-title (frame pane)
  (centering-output (pane :vertically t :horizontally t)
    (with-text-size (pane :large)
      (with-text-face (pane :bold)
        (princ (progress-view-operation frame) pane)))))

(defun display-progress-message (frame pane)
  (centering-output (pane :vertically t :horizontally t)
    (princ (progress-view-message frame) pane)))

(defun display-progress-completion (frame pane)
  (with-bounding-rectangle* (sx0 sy0 sx1 sy1) (sheet-region pane)
    (let* ((completion (min 1 (max 0 (progress-view-completion frame))))
           (width (- sx1 sx0))
           (height (- sy1 sy0))
           (fill (* completion width))
           (text (format nil "~D %" (floor (* 100 completion))))
           (style (make-text-style :sans-serif :roman :normal)))
      (draw-rectangle* pane 0 0 fill height :ink +black+)
      (draw-text* pane text (/ width 2) (/ height 2) :align-x :center :align-y :center
                  :ink +white+ :text-style style))))

(defun compute-progress-bar-length (frame pane)
  (with-bounding-rectangle* (sx0 sy0 sx1 sy1) (sheet-region pane)
    (declare (ignore sy0 sy1))
    (let* ((completion (min 1 (max 0 (progress-view-completion frame))))
           (width (- sx1 sx0)))
      (nth-value 0 (floor (* completion width))))))

(defun invoke-with-progress (function 
                             &key (width 790) (height 550)
                                  port frame-manager (process-name "Progress"))
  (let* ((fm (or frame-manager (find-frame-manager :port (or port (find-port)))))
         (frame (make-application-frame 'progress-view
                                        :pretty-name process-name
                                        :frame-manager fm
                                        :width width
                                        :height height))
         (queue (cons nil nil)))
    (labels 
        ((update-display ()
           (loop
              (let ((object (car queue)))
                (when object
                  (macrolet ((update (pane accessor value)
                               `(let ((.old. (,accessor frame))
                                      (.new. ,value))
                                  (unless (equal .old. .new.)
                                    (setf (,accessor frame) .new.)
                                    (setf (pane-needs-redisplay (get-frame-pane frame ',pane)) t)
                                    t))))
                    (let* ((operation (progress-operation object))
                           (phase (progress-phase object))
                           (action (progress-action object))
                           (message (progress-message object))
                           (completion (progress-completion object))
                           (progress-bar (get-frame-pane frame 'completion))
                           (new-bar-length (compute-progress-bar-length frame progress-bar))
                           (stuff (append (when phase (list phase)) (when action (if (listp action) action (list action)))))
                           (text (format nil "~@[~{~A~^ ~}~]~@[~A~]~@[~A~]" stuff (and stuff message ": ") message)))
                      (setf (progress-view-completion frame) completion)
                      (when (or (update title progress-view-operation operation)
                                (update message progress-view-message text)
                                (update completion progress-bar-length new-bar-length))
                        (redisplay-frame-panes frame)))))
                (when (eq object (sb-ext:compare-and-swap (car queue) object nil))
                  (return)))))
         (handle (object)
           (loop
              (let ((old (car queue)))
                (when (eq old (sb-ext:compare-and-swap (car queue) old object))
                  (unless old (send #'update-display))
                  (return)))))
         (send (action &rest args)
           (invoke-in-frame-event-loop action frame :arguments (copy-list args)))
         (run-frame () 
           (unwind-protect (run-frame-top-level frame)
             (disown-frame fm frame))))
      (clim-sys:make-process #'run-frame :name process-name)
      (unwind-protect (handler-bind ((progress #'handle)) 
                        (let ((*signal-progress* t))
                          (funcall function)))
        (send #'frame-exit frame))))
  nil)


(defmacro with-progress ((&rest options) &body body)
  `(invoke-with-progress (lambda () ,@body)
                         ,@options))
