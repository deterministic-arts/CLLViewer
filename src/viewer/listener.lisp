
(in-package "CLL-VIEWER")

(defgeneric listener-store (object))
(defgeneric (setf listener-store) (value object))
(defgeneric listener-selection (object))
(defgeneric (setf listener-selection) (value object))
(defgeneric listener-stack (object))

(define-application-frame listener (conditional-command-support standard-application-frame)
  ((store
     :type (or null store) :initform nil
     :accessor %listener-store :reader node-store :reader listener-store)
   (stack
     :type list :initform nil
     :accessor %listener-stack :reader listener-stack))
  (:command-table (listener :inherit-from (global-commands)))
  (:menu-bar nil)
  (:panes
    (header :application
      :height '(3 :line) :min-height '(3 :line) :max-height '(3 :line)
      :name 'header
      :border-width 0
      :background +white+
      :display-function 'display-header
      :scroll-bars nil
      :display-time :command-loop
      ;;:default-view +tree-view+
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (toolbox :application
      :height '(2 :line) :min-height '(2 :line) :max-height '(2 :line)
      :name 'toolbox
      :border-width 0
      :background +white+
      :display-function 'display-toolbox
      :scroll-bars nil
      :display-time :command-loop
      ;;:default-view +tree-view+
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (dates :application 
      :height '(3 :line) :min-height '(3 :line) :max-height '(3 :line)
      :name 'dates
      :border-width 0
      :background +white+
      :display-function 'display-dates
      :scroll-bars nil
      :display-time :command-loop
      ;;:default-view +tree-view+
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (all-threads  :application 
      :name 'all-threads
      :border-width 0
      :background +white+
      :display-function 'display-thread-list
      :scroll-bars t
      :display-time :command-loop
      ;;:default-view +tree-view+
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (threads-adjuster (make-pane 'clim-extensions:box-adjuster-gadget :background +gray60+ :border-style :solid :border-width 1))
    (current-thread :application 
      :name 'current-thread
      :border-width 0
      :background +white+
      :display-function 'display-current-thread
      :scroll-bars t
      :display-time :command-loop
      ;;:default-view +tree-view+
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (tree-adjuster (make-pane 'clim-extensions:box-adjuster-gadget :background +gray60+ :border-style :solid :border-width 1))
    (primary :application
      :name 'primary
      :border-width 0
      :background +white+
      :display-time :command-loop
      :display-function 'display-primary
      :scroll-bars t
      ;;:default-view +primary-view+
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (primary-adjuster (make-pane 'clim-extensions:box-adjuster-gadget :background +gray60+ :border-style :solid :border-width 1))
    (interactor*
      (make-clim-stream-pane 
        :type 'interactor-pane
        :name 'interactor
        ;;:default-view +listener-view+
        :border-width 0
        :scroll-bars t))
    (documentation :pointer-documentation))
  (:layouts 
    (default 
      (spacing (:thickness 3 :background +white+)
        (vertically (:y-spacing 3 :background +white+)
          (horizontally (:x-spacing 3 :background +white+)
            (vertically (:y-spacing 3 :background +white+)
              (outlining (:thickness 1 :foreground +black+) 
                (spacing (:thickness 3 :background +white+) dates))
              (outlining (:thickness 1 :foreground +black+) 
                all-threads)
              threads-adjuster
              (outlining (:thickness 1 :foreground +black+)
                current-thread))
            tree-adjuster
            (vertically (:x-spacing 3 :y-spacing 3 :background +white+)
              (outlining (:thickness 1 :foreground +black+) 
                (spacing (:thickness 3 :background +white+) 
                  header))
              toolbox
              (outlining (:thickness 1 :foreground +black+) primary)))
          primary-adjuster
          (outlining (:thickness 1 :foreground +black+) interactor*)
          documentation)))))

(defmethod listener-selection ((object listener))
  (car (%listener-stack object)))

(defmethod (setf listener-selection) (value (object listener))
  (setf (%listener-stack object) (and value (node-path value)))
  value)

(defmethod (setf listener-store) (value (object listener))
  (setf (%listener-store object) value))

(defmethod (setf listener-store) :after (value (object listener))
  (if (not value)
      (setf (%listener-stack object) nil)
      (let* ((section (find-node :threads value))
             (date-range (car (section-date-range-list section)))
             (thread (car (date-range-threads date-range))))
        (setf (%listener-stack object) (node-path thread)))))



(defmethod frame-standard-output ((frame listener))
  (get-frame-pane frame 'interactor))

(defmethod frame-query-io ((frame listener))
  (get-frame-pane frame 'interactor))

(define-listener-command (com-quit-application :name "Quit Application") ()
  (frame-exit *application-frame*))

(define-listener-command (com-clear-console :name "Clear Console") ()
  (window-clear *standard-output*))

(defmethod execute-frame-command :around ((frame listener) command)
  (declare (ignore command))
  (with-general-error-handler ()
    (call-next-method)))

(defun run-listener (&key (new-process nil) (width 790) (height 550)
                          port frame-manager (process-name "CLL Viewer"))
  (let* ((fm (or frame-manager (find-frame-manager :port (or port (find-port)))))
         (frame (make-application-frame 'listener
                                        :pretty-name process-name
                                        :frame-manager fm
                                        :width width
                                        :height height)))
    (flet ((run () 
             (unwind-protect (unwind-protect (run-frame-top-level frame)
                               (let ((connection (listener-store frame)))
                                 (setf (listener-store frame) nil)
                                 (when connection
                                   (close-store connection))))
               (disown-frame fm frame))))
      (if new-process
          (values (clim-sys:make-process #'run :name process-name) frame)
          (run)))))

(define-command (com-open-store 
                  :enabled-if (lambda (frame) (not (listener-store frame)))
                  :name "Open Store"
                  :command-table listener) ((path '((pathname) :default-type "txt")))
  (let* ((frame *application-frame*)
         (store (open-store path)))
    (setf (listener-store frame) store)))

(define-command (com-close-store 
                  :enabled-if listener-store
                  :name "Close Store"
                  :command-table listener) ()
  (let ((cnx (listener-store *application-frame*)))
    (setf (listener-store *application-frame*) nil)
    (close-store cnx)))



(define-command (com-select-date-range 
                  :name "Select Date Range"
                  :enabled-if listener-store
                  :command-table global-commands) ((range 'date-range :gesture (:select
                                                                               :documentation "Select")))
  (setf (listener-selection *application-frame*) (car (date-range-threads range))))

(define-command (com-select-node
                  :name "Select Node"
                  :enabled-if listener-store
                  :command-table global-commands) ((object 'node :gesture (:select 
                                                                           :documentation "Select")))
  (setf (listener-selection *application-frame*) object))

(define-command (com-show-headers
                  :name "Show Headers"
                  :command-table global-commands
                  :enabled-if listener-store) ((object 'node :gesture (:menu
                                                                       :documentation "Show Headers"
                                                                       :tester ((object) (messagep object)))))
  (let ((headers (message-headers object)))
    (when headers
      (formatting-table (t :x-spacing "WW")
        (with-text-face (t :bold)
          (formatting-row (t)
            (formatting-cell (t) (write-string "Key"))
            (formatting-cell (t) (write-string "Value"))))
        (loop
          :for (key . value) :in headers
          :do (formatting-row (t)
                (formatting-cell (t) (write-string key))
                (formatting-cell (t) (write-string value))))))))



(define-presentation-type date-range ())

(define-presentation-type current-date-range ()
  :inherit-from 'date-range)

(define-presentation-type store ())

(define-presentation-type current-store ()
  :inherit-from 'store)

(define-command (com-pick-date-range
                  :command-table global-commands
                  :enabled-if listener-store
                  :name "Pick Date Range") ()
  (let* ((choices (section-date-range-list (listener-selection *application-frame*)))
         (small (make-text-style nil :roman :small))
         (range (menu-choose choices 
                             :label "Date Range"
                             :n-columns 4
                             :x-spacing "WW" :y-spacing 6
                             :printer (lambda (object stream)
                                        (multiple-value-bind (sec min hour day month year) (decode-universal-time (date-range-start object) 0)
                                          (declare (ignore sec min hour day))
                                          (with-text-face (stream (if (eql month 1) :bold :roman))
                                            (format stream "~A ~D"
                                                    (aref '#("" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") 
                                                          month)
                                                    year))
                                          (terpri stream)
                                          (with-text-style (stream small)
                                            (format stream "~D message~:*~P" (date-range-count object))))))))
    (when range
      (let ((selection (car (date-range-threads range))))
        (setf (listener-selection *application-frame*) selection)))))

(define-presentation-to-command-translator pick-date-range-translator
    (current-date-range com-pick-date-range global-commands
      :gesture :select
      :priority 100
      :documentation ((object stream)
                      (declare (ignore object))
                      (format stream "Pick a date range")))
    (object)
  (progn object nil))

(define-presentation-to-command-translator close-current-store-translator 
    (current-store com-close-store listener
      :gesture :menu 
      :documentation "Close")
    (object)
  (progn object nil))



(defparameter +article-text-style+
  (make-text-style :fix :roman :large)) 

(defun display-dates (frame pane)
  (let* ((selection (listener-selection frame))
         (range (node-section-date-range selection))
         (section (node-section selection)))
    (formatting-table (pane :x-spacing "WW")
      (formatting-row (pane)
        (formatting-cell (pane) (with-text-face (pane :bold) (write-string "Store" pane)))
        (formatting-cell (pane)
          (when selection
            (with-output-as-presentation (pane (node-store selection) 'current-store :single-box t)
              (let ((path1 (darts.lib.sqlite-connection:connection-pathname (node-store selection)))
                    (path2 (message-file (node-store selection))))
                (format pane "~A.~A / ~A.~A" 
                        (pathname-name path1) (pathname-type path1)
                        (pathname-name path2) (pathname-type path2)))))))
      (formatting-row (pane)
        (formatting-cell (pane) (with-text-face (pane :bold) (write-string "Section" pane)))
        (formatting-cell (pane)
          (typecase section
            (orphans-node (write-string "Orphans" pane))
            (threads-node (write-string "Threads" pane))
            (t nil))))
      (formatting-row (pane)
        (formatting-cell (pane) (with-text-face (pane :bold) (write-string "Month" pane)))
        (formatting-cell (pane)
          (when range
            (multiple-value-bind (sec min hour day month year) (decode-universal-time (date-range-start range) 0)
              (declare (ignore sec min hour day))
              (with-output-as-presentation (pane range 'current-date-range :single-box t)
                (format pane "~A ~D (~D)"
                        (aref '#("" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") 
                              month)
                        year (date-range-count range)))))))
        nil)))
              

(defun display-toolbox (frame pane)
  (let* ((selection (listener-selection frame))
         (successor (and selection (node-successor selection)))
         (predecessor (and selection (node-predecessor selection)))
         (parent (and selection (node-parent selection))))
    (centering-output (pane :vertically t :horizontally nil)
    (labels 
        ((show-button (caption object)
           (formatting-cell (pane)
             (if (not object)
                 (with-drawing-options (pane :ink +gray30+) 
                   (write-string caption pane))
                 (with-output-as-presentation (pane `(com-select-node ,object) 'command :single-box t)
                   (write-string caption pane))))))
      (formatting-table (pane :x-spacing "WW")
        (formatting-row (pane)
          (show-button "Previous" predecessor)
          (show-button "Parent" parent)
          (show-button "Next" successor)))))))

      
(defun display-header (frame pane)
  (let ((selection (listener-selection frame)))
    (formatting-table (pane :x-spacing "WW")
      (formatting-row (pane)
        (formatting-cell (pane) (with-text-face (pane :bold) (write-string "Date" pane)))
        (formatting-cell (pane) 
          (when (messagep selection)
            (multiple-value-bind (sec min hour day month year) (decode-universal-time (message-date selection) 0)
              (declare (ignore sec))
              (format pane "~4,'0D-~2,'0D-~2,'0D, ~2,'0D:~2,'0D UTC"
                      year month day hour min)))))
      (formatting-row (pane)
        (formatting-cell (pane) (with-text-face (pane :bold) (write-string "Author" pane)))
        (formatting-cell (pane) 
          (when (messagep selection)
            (format pane "~A" (or (message-author selection) "")))))
      (formatting-row (pane)
        (formatting-cell (pane) (with-text-face (pane :bold) (write-string "Subject" pane)))
        (formatting-cell (pane)
          (when selection
            (format pane "~A" (or (node-title selection) ""))))))))


(defun display-thread-list (frame pane)
  (let* ((selection (listener-selection frame))
         (range (and selection (node-section-date-range selection)))
         (path (and selection (listener-stack frame))))
    (when range
      (let ((threads (date-range-threads range)))
        (loop
          :for object :in threads
          :for face := (if (member object path) :bold :roman)
          :do (fresh-line pane)
              (with-output-as-presentation (pane object 'node :single-box t)
                (with-text-face (pane face)
                (format pane "~A~@[ (~D)~]" 
                        (or (node-title object) "(Unknown)")
                        (and (plusp (node-descendant-count object))
                             (node-descendant-count object))))))))))
 

(defun display-primary (frame pane)
  (let* ((selection (listener-selection frame))
         (text (and (messagep selection) (message-text selection))))
    (window-clear pane)
    (when text
      (with-output-as-presentation (pane selection 'message :single-box t)
        (with-text-style (pane +article-text-style+)
          (let ((lines (mapcar (lambda (line) (expand-tabs line 8))
                               (split-sequence #\newline text :remove-empty-subseqs nil))))
            (dolist (line lines)
              (with-drawing-options (pane :ink (if (scan "^\\s*([>|]|<\\s).*" line) +gray40+ +black+))
              (let ((start 0))
                (loop
                  (multiple-value-bind (mstart mend) (scan "\\b(https?://[^/]+(?:/[a-zA-Z0-9!$%&/()=?*+~#_.:;-]*)?)" line :start start)
                    (if (not mstart)
                        (progn 
                          (write-string line pane :start start) 
                          (return))
                        (progn
                          (when (< start mstart) (write-string line pane :start start :end mstart))
                          (let ((uri (ignore-errors (parse-uri (subseq line mstart mend)))))
                            (if (not uri)
                                (write-string line pane :start mstart :end mend)
                                (with-output-as-presentation (pane uri 'uri :single-box t)
                                  (with-drawing-options (pane :ink +blue+)
                                    (write-string line pane :start mstart :end mend)))))
                          (setf start mend)))))))
              (terpri pane))))))))




(defun display-current-thread (frame pane)
  (let* ((selection (listener-selection frame))
         (root (and selection (node-thread selection)))
         (pixels-per-level 16))
    (when root
      (labels
          ((highlightp (object) (eql object selection))
           (show (object level)
             (fresh-line pane)
             (stream-increment-cursor-position pane (* level pixels-per-level) 0)
             (with-output-as-presentation (pane object 'node :single-box t)
               (with-text-face (pane (if (highlightp object) :bold :roman))
                 (format pane "~@[~A~]~@[ by ~A~]"
                         (node-title object) 
                         (and (messagep object) (message-author object)))))
             (when (plusp (node-child-count object))
               (map-over-child-nodes (lambda (child) (show child (1+ level)))
                                     object))))
        (show root 0)))))

             
      
