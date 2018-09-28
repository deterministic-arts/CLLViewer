
(in-package "CLL-VIEWER")

(defgeneric listener-store (object))
(defgeneric (setf listener-store) (value object))
(defgeneric listener-selection (object))
(defgeneric (setf listener-selection) (value object))
(defgeneric listener-stack (object))
(defgeneric listener-memory (object))
(defgeneric (setf listener-memory) (value object))

(define-application-frame listener (conditional-command-support standard-application-frame)
  ((store
     :type (or null store) :initform nil
     :accessor %listener-store :reader node-store :reader listener-store)
   (stack
     :type list :initform nil
     :accessor %listener-stack :reader listener-stack)
   (memory 
     :type list :initform  nil
     :accessor listener-memory))
  (:command-table (listener :inherit-from (global-commands)))
  (:menu-bar nil)
  (:panes
    (toolbox :application
      :height 27 :min-height 26 :max-height 26
      :name 'toolbox
      :borders nil
      :background +gray80+
      :display-function 'display-toolbox
      :scroll-bars nil
      :display-time :command-loop
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (dates :application 
      :height '(3 :line) :min-height '(3 :line) :max-height '(3 :line)
      :name 'dates
      :borders nil
      :background +white+
      :display-function 'display-dates
      :scroll-bars nil
      :display-time :command-loop
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (all-threads  :application 
      :name 'all-threads
      :borders nil
      :background +white+
      :display-function 'display-thread-list
      :scroll-bars t
      :display-time :command-loop
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (memory-adjuster (make-pane 'clim-extensions:box-adjuster-gadget))
    (current-thread :application 
      :name 'current-thread
      :borders nil
      :background +white+
      :display-function 'display-current-thread
      :scroll-bars t
      :display-time :command-loop
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (tree-adjuster (make-pane 'clim-extensions:box-adjuster-gadget))
    (primary :application
      :name 'primary
      :borders nil
      :background +white+
      :display-time :command-loop
      :display-function 'display-primary
      :scroll-bars nil
      :end-of-line-action :allow
      :end-of-page-action :allow)
    (primary-adjuster (make-pane 'clim-extensions:box-adjuster-gadget))
    (interactor*
      (make-clim-stream-pane 
        :type 'interactor-pane
        :name 'interactor
        :borders nil
        :scroll-bars t))
    (documentation :pointer-documentation))
  (:layouts 
    (default 
      (spacing (:thickness 3 :background +gray80+)
        (vertically (:y-spacing 3 :background +gray80+)
          (horizontally (:x-spacing 3 :background +gray80+)
            (vertically (:y-spacing 3 :background +gray80+)
              (climi::lowering ()
                (spacing (:thickness 1 :background +gray80+)
                  (restraining () dates)))
              (climi::lowering ()
                (spacing (:thickness 1 :background +gray80+)
                  (restraining () all-threads))))
            tree-adjuster
            (vertically (:x-spacing 3 :y-spacing 3 :background +gray80+)
              (climi::lowering ()
                (spacing (:thickness 1 :background +gray80+)
                  (restraining ()
                    (scrolling (:scroll-bar t)
                      primary))))
              toolbox)
            memory-adjuster
            (climi::lowering ()
              (spacing (:thickness 1 :background +gray80+)
                current-thread)))
          primary-adjuster
          (climi::lowering ()
            (spacing (:thickness 1 :background +gray80+)
              interactor*))
          documentation)))))

(defmethod listener-selection ((object listener))
  (car (%listener-stack object)))

(defmethod (setf listener-selection) (value (object listener))
  (setf (%listener-stack object) (and value (node-path value)))
  value)

(defmethod (setf listener-store) (value (object listener))
  (setf (%listener-store object) value))

(defmethod (setf listener-store) :after (value (object listener))
  (setf (listener-memory object) nil)
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



(define-command (com-remember-node
                  :command-table global-commands
                  :enabled-if listener-store
                  :name nil)
    ((node 'node :gesture (:menu
                           :tester ((object) (not (member object (listener-memory *application-frame*))))
                           :documentation "Remember")))
  (push node (listener-memory *application-frame*)))

(define-command (com-forget-node 
                  :command-table global-commands
                  :enabled-if listener-store
                  :name nil)
    ((node 'node :gesture (:menu
                           :tester ((object) (member object (listener-memory *application-frame*)))
                           :documentation "Forget")))
  (setf (listener-memory *application-frame*) 
        (remove node (listener-memory *application-frame*))))

(define-command (com-select-date-range 
                  :name nil
                  :enabled-if listener-store
                  :command-table global-commands)
    ((range 'date-range :gesture (:select
                                  :documentation "Select")))
  (setf (listener-selection *application-frame*) (car (date-range-threads range))))

(define-command (com-select-node
                  :name nil
                  :enabled-if listener-store
                  :command-table global-commands)
    ((object 'node :gesture (:select 
                             :documentation "Select")))
  (setf (listener-selection *application-frame*) object))

(define-command (com-show-headers
                  :name nil
                  :command-table global-commands
                  :enabled-if listener-store)
    ((object 'node :gesture (:menu
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

(define-command (com-list-bookmarks 
                  :command-table global-commands
                  :enabled-if listener-store
                  :name "List Bookmarks") ()
  (let ((all (list-bookmarks (listener-store *application-frame*))))
    (if (not all)
        (format t "~&There are not bookmarks.")
        (formatting-table (t :x-spacing "WW")
          (formatting-row (t)
            (with-text-face (t :bold)
              (formatting-cell (t) (write-string "Message"))
              (formatting-cell (t) nil)
              (formatting-cell (t) (write-string "Description"))))
          (dolist (object all)
            (with-output-as-presentation (t object 'bookmark :single-box t)
            (formatting-row (t)
              (with-output-as-presentation (t (bookmark-node object) 'node :single-box t)
                (formatting-cell (t) (format t "#~D" (node-key (bookmark-node object))))
                (formatting-cell (t) (format t "~@[~A~]" (node-title (bookmark-node object)))))
              (formatting-cell (t)
                (format t "~@[~A~]" (bookmark-description object))))))))))


(define-command (com-delete-bookmark 
                  :command-table global-commands
                  :name nil
                  :enabled-if listener-store)
    ((object 'bookmark :gesture (:delete :documentation "Delete")))
  (delete-bookmark object))


(define-command (com-add-bookmark 
                  :enabled-if listener-store
                  :command-table global-commands
                  :name nil)
    ((object 'node :gesture (:menu :documentation "Add Bookmark"))
     &key (priority 'integer :prompt "priority" :default 0) 
          (description 'string :prompt "description"))
  (update-bookmark object
                   :description description
                   :priority priority))

(define-command (com-annotate-bookmark
                  :enabled-if listener-store
                  :command-table global-commands
                  :name "Annotate Bookmark") ((object 'bookmark :prompt "bookmark")
                                              (description 'string :prompt "description"))
  (let* ((description (string-trim #.(concatenate 'string '(#\space #\tab #\newline #\return)) description))
         (true-description (if (zerop (length description)) nil description)))
    (update-bookmark* (bookmark-node object) (list :description true-description))))

(define-command (com-interactively-annotate-bookmark 
                  :enabled-if listener-store
                  :command-table global-commands
                  :name nil)
    ((object 'bookmark :gesture (:edit :documentation "Annotate")))
  (let* ((description (string-trim #.(concatenate 'string '(#\space #\tab #\newline #\return)) (accept 'string :prompt "description")))
         (true-description (if (zerop (length description)) nil description)))
    (update-bookmark* (bookmark-node object) (list :description true-description))))



(defun invoke-preserving-nodes (function &optional (frame *application-frame*))
  (let ((selection (listener-selection frame))
        (store (listener-store frame))
        (memory (listener-memory frame))
        (result (multiple-value-list (funcall function))))
    (when (eq store (listener-store frame))
      (let ((new-memory (mapcan (lambda (object)
                                  (let ((new-object (find-node (node-key object) store nil nil)))
                                    (when new-object
                                      (list new-object))))
                                memory))
            (new-selection (and selection (let ((node (find-node (node-key selection) store nil nil)))
                                            (or node
                                                (let* ((section (find-node :threads store))
                                                       (ranges (section-date-range-list section)))
                                                  (and ranges
                                                       (car (date-range-threads (car ranges))))))))))
        (setf (listener-memory frame) new-memory)
        (setf (listener-selection frame) new-selection)))
    (values-list result)))

(defmacro preserving-nodes ((&rest options) &body body)
  `(invoke-preserving-nodes (lambda () ,@body) ,@options))


(define-command (com-update-message-counters
                  :command-table global-commands
                  :enabled-if listener-store
                  :name "Update Message Counters") ()
  (window-clear *standard-output*)
  (let ((frame *application-frame*))
    (preserving-nodes (frame)
      (let ((store (listener-store frame)))
        (with-simple-progress (t :erase-view t)
          (cll-indexer:update-message-counters store)
          (flush-message-caches store))))))


(define-command (com-reparent-node 
                  :command-table global-commands
                  :enabled-if listener-store
                  :name "Reparent Node") ((child 'node) (parent 'node))
  (window-clear *standard-output*)
  (preserving-nodes ()
    (let ((store (node-store child)))
      (reparent-node child parent)
      (with-simple-progress (t :erase-view t)
        (cll-indexer:update-message-counters store :children-only t)
        (flush-message-caches store)))))


(define-command (com-interactively-reparent-node
                  :command-table global-commands
                  :enabled-if listener-store
                  :name nil)
    ((child 'node :gesture (:menu :documentation "Reparent")))
  (com-reparent-node child (accept 'node)))


(define-command (com-mark-as-spam 
                  :command-table global-commands
                  :enabled-if listener-store
                  :name nil)
    ((object 'node :gesture (:delete 
                             :tester ((object)
                                      (and (typep object 'thread-root-message)
                                           (not (find-if (lambda (elt) (typep elt 'spam-node))
                                                         (node-path object)))))
                             :documentation "Mark As Spam")))
  (preserving-nodes ()
    (let* ((store (listener-store *application-frame*))
           (spam (find-node :spam store)))
      (reparent-node object spam))))

(define-command (com-mark-as-spam-and-go-to-next
                  :command-table global-commands
                  :enabled-if listener-store
                  :name nil)
    ((object 'node :gesture (:delete
                             :menu nil
                             :tester ((object)
                                      (and (typep object 'thread-root-message)
                                           (not (find-if (lambda (elt) (typep elt 'spam-node))
                                                         (node-path object)))))
                             :documentation "Mark As Spam")))
  (let ((next (node-successor object))
        (prev (node-predecessor object))
        (parent (node-parent object)))
    (let ((selection (or next prev parent)))
      (setf (listener-selection *application-frame*) selection)
      (com-mark-as-spam object))))    

(define-command (com-move-to-threads
                  :command-table global-commands
                  :enabled-if listener-store
                  :name nil)
    ((object 'node :gesture (:menu 
                             :tester ((object)
                                      (and (typep object 'thread-root-message)
                                           (not (find-if (lambda (elt) (typep elt 'threads-node))
                                                         (node-path object)))))
                             :documentation "Move To Threads")))
  (preserving-nodes ()
    (let* ((store (listener-store *application-frame*))
           (spam (find-node :threads store)))
      (reparent-node object spam))))



(define-presentation-type date-range ())

(define-presentation-type current-date-range ()
  :inherit-from 'date-range)

(define-presentation-type store ())

(define-presentation-type current-store ()
  :inherit-from 'store)

(define-presentation-type current-section ()
  :inherit-from 'node)


(defun draw-range-year (year ranges sensitizer stream)
  (format stream "~&~D~%" year)
  (formatting-table (stream :x-spacing "WW")
    (loop
      :for row :upfrom 0 :below 4
      :do (formatting-row (stream)
            (loop
              :for col :upfrom 0 :below 3
              :for month := (+ 1 (* row 3) col)
              :do (formatting-cell (stream)
                    (let* ((head (car ranges))
                           (count (and head (date-range-count head)))
                           (match (and head (multiple-value-bind (sec min hour day rmonth ryear) (decode-universal-time (date-range-start head) 0)
                                              (declare (ignore sec min hour day))
                                              (and (eql year ryear) (eql month rmonth)))))
                           (name (aref '#("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)))
                      (if (not match)
                          (format stream "~A" name)
                          (progn
                            (funcall sensitizer head stream (lambda () (format stream "~A (~D)" name count)))
                            (setf ranges (cdr ranges))))))))))
  ranges)

(defun draw-date-ranges (ranges stream &key sensitizer)
  (loop
    :while ranges
    :do (multiple-value-bind (sec min hour day month year) (decode-universal-time (date-range-start (car ranges)) 0)
          (declare (ignore sec min hour day month))
          (setf ranges (draw-range-year year ranges 
                                        (or sensitizer (lambda (object stream continuation)
                                                         (with-output-as-presentation (stream object 'date-range :single-box t)
                                                           (funcall continuation))))
                                        stream)))))


(define-command (com-show-date-ranges 
                  :command-table global-commands
                  :enabled-if listener-store
                  :name "Show Date Ranges") ()
  (draw-date-ranges (section-date-range-list (listener-selection *application-frame*))
                    *standard-output*))

(define-command (com-select-section 
                  :command-table global-commands
                  :enabled-if listener-store
                  :name "Select Section") ((key '(member :orphans :threads :spam)))
  (let* ((store (listener-store *application-frame*))
         (root (find-node key store nil nil)))
    (when root
      (let* ((range (car (section-date-range-list root)))
             (thread (and range (car (date-range-threads range)))))
        (setf (listener-selection *application-frame*) thread)))))


(define-command (com-pick-section 
                  :command-table global-commands
                  :enabled-if listener-selection
                  :name "Pick Section") ()
  (let* ((choices '(:threads :orphans :spam))
         (choice (menu-choose choices 
                              :label "Section"
                              :printer (lambda (object stream) 
                                         (write-string (string-capitalize (symbol-name object)) stream)))))
    (when choice
      (com-select-section choice))))


(define-command (com-pick-date-range
                 :command-table global-commands
                 :enabled-if listener-store
                 :name "Pick Date Range") ()
  (let* ((choices (section-date-range-list (listener-selection *application-frame*)))
         (current (node-section-date-range (listener-selection *application-frame*))) 
         (range (menu-choose choices 
                             :label "Date Range"
                             :n-columns 6
                             :x-spacing "WW" :y-spacing 6
                             :printer (lambda (object stream)
                                        (let* ((date (date-range-start object))
                                               (year (local-year date))
                                               (month (local-month date)))
                                          (with-drawing-options (stream :ink (if (eq object current) +blue+ +black+))
                                            (with-text-size (stream :small)
                                              (with-text-face (stream (if (eql month 1) :bold :roman))
                                                (format stream "~A~@[ ~D~] (~D)"
                                                        (aref '#("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)
                                                        year
                                                        (date-range-count object))))))))))
    (when range
      (let ((selection (car (date-range-threads range))))
        (setf (listener-selection *application-frame*) selection)))))


(define-command (com-pick-bookmark
                  :enabled-if listener-store
                  :name "Pick Bookmark"
                  :command-table listener)
    ()
  (let* ((store (listener-store *application-frame*))
         (bookmarks (list-bookmarks store)))
    (when bookmarks
      (let ((selection (menu-choose bookmarks
                                    :label "Bookmark"
                                    :printer (lambda (object stream)
                                               (format stream "~A by ~A~@[: ~A~]"
                                                       (or (node-title (bookmark-node object)) "(Unknown)")
                                                       (or (and (messagep (bookmark-node object)) (message-author (bookmark-node object))) "(Unknown)")
                                                       (bookmark-description object))))))
        (when selection
          (com-select-node (bookmark-node selection)))))))


(define-presentation-to-command-translator pick-date-range-translator
    (current-date-range com-pick-date-range global-commands
      :gesture :select
      :priority 100
      :documentation ((object stream)
                      (declare (ignore object))
                      (format stream "Pick a date range")))
    (object)
  (progn object nil))

(define-presentation-to-command-translator pick-section-translator 
    (current-section com-pick-section global-commands
      :gesture :select
      :priority 1000
      :documentation "Pick Section")
    (object)
  (progn object nil))

(define-presentation-to-command-translator close-current-store-translator 
    (current-store com-close-store listener
      :gesture :menu 
      :documentation "Close")
    (object)
  (progn object nil))

(define-presentation-to-command-translator goto-bookmarked-node-translator
    (bookmark com-select-node global-commands
      :gesture :select
      :documentation "Go To")
    (object)
  (list (bookmark-node object)))



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
          (when section 
            (with-output-as-presentation (pane section 'current-section :single-box t)
              (typecase section
                (orphans-node (write-string "Orphans" pane))
                (threads-node (write-string "Threads" pane))
                (spam-node (write-string "Spam" pane))
                (t nil))))))
      (formatting-row (pane)
        (formatting-cell (pane) (with-text-face (pane :bold) (write-string "Month" pane)))
        (formatting-cell (pane)
          (when range
            (let ((date (date-range-start range)))
              (with-output-as-presentation (pane range 'current-date-range :single-box t)
                (format pane "~A ~D (~D)"
                        (aref '#("" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") 
                              (local-month date))
                        (local-year date)
                        (date-range-count range)))))))
        nil)))
              

(defun draw-symbol-box (stream symbol x1 y1 x2 y2
                        &key
                          (ink +foreground-ink+) (outline-ink ink) symbol-size
                          (background-ink +background-ink+))
  (with-sheet-medium (medium stream)
    (let ((text (string (clim-symbol-font:symbol-character symbol)))
          (style (clim-symbol-font:symbol-text-style :stream stream :size symbol-size)))
      (multiple-value-bind (tw th) (text-size medium text :text-style style)
        (when background-ink
          (draw-rectangle* stream x1 y1 x2 y2 :filled t :ink background-ink))
        (draw-text* stream text (- (/ (+ x1 x2) 2) (/ tw 2)) (- (/ (+ y1 y2) 2) (/ th 2))
                    :ink ink :text-style style :align-x :left :align-y :top)
        (when outline-ink
          (draw-rectangle* stream x1 y1 x2 y2 :filled nil :ink outline-ink))))))


(defun display-toolbox (frame pane)
  (let* ((selection (listener-selection frame))
         (successor (and selection (node-successor selection)))
         (predecessor (and selection (node-predecessor selection)))
         (parent (and selection (node-parent selection))))
    (centering-output (pane :vertically t :horizontally nil)
      (labels 
          ((paint (caption active bgink)
             (let ((fg (if active +black+ +gray30+)))
               (draw-symbol-box pane caption 0 0 22 22
                                :ink fg :background-ink bgink
                                :outline-ink nil)))
           (show-button (caption command &key (background-ink +transparent-ink+))
             (formatting-cell (pane)
               (if (not command)
                   (paint caption nil background-ink)
                   (with-output-as-presentation (pane command 'command :single-box t)
                     (paint caption t background-ink)))))
           (show-select-button (caption object)
             (formatting-cell (pane)
               (if (not object)
                   (paint caption nil +transparent-ink+)
                   (with-output-as-presentation (pane `(com-select-node ,object) 'command :single-box t)
                     (paint caption t +transparent-ink+))))))
        (formatting-table (pane :x-spacing 3)
          (formatting-row (pane)
            (show-select-button :arrow-left predecessor)
            (show-select-button :arrow-up parent)
            (show-select-button :arrow-right successor)
            (show-button :tags (and (listener-store frame) `(com-pick-bookmark)))
            (formatting-cell (pane) (write-string "    " pane))
            (let* ((mark (and (listener-store frame) selection (find-bookmark (message-identifier selection) (listener-store frame))))
                   (cmd (cond
                          ((not selection) nil)
                          ((not mark) `(com-add-bookmark ,selection))
                          (t `(com-delete-bookmark ,mark)))))
              (show-button :bookmark cmd
                           :background-ink (if mark +gray95+ +transparent-ink+)))
            (show-button :trash (and selection `(com-mark-as-spam-and-go-to-next ,selection)))))))))

      
(defun display-header (selection pane)
  (formatting-table (pane :x-spacing "WW")
    (formatting-row (pane)
      (formatting-cell (pane) (with-text-face (pane :bold) (write-string "Date" pane)))
      (formatting-cell (pane) 
        (when (messagep selection)
          (print-local-date-time (message-date selection) pane))))
    (formatting-row (pane)
      (formatting-cell (pane) (with-text-face (pane :bold) (write-string "Author" pane)))
      (formatting-cell (pane) 
        (when (messagep selection)
          (format pane "~A" (or (message-author selection) "")))))
    (formatting-row (pane)
      (formatting-cell (pane) (with-text-face (pane :bold) (write-string "Subject" pane)))
      (formatting-cell (pane)
        (when selection
          (format pane "~A" 
                  (let* ((text (or (node-title selection) ""))
                         (length (length text)))
                    (if (<= length 70) text
                        (subseq text 0 70)))))))))

(defun display-thread-list (frame pane)
  (let* ((selection (listener-selection frame))
         (range (and selection (node-section-date-range selection)))
         (path (and selection (listener-stack frame)))
         focus-record)
    (when range
      (let ((threads (date-range-threads range)))
        (when threads
          (formatting-table (pane)
            (loop
               :for object :in threads
               :for active := (member object path)
               :for face := (if active :bold :roman)
               :for ink := (if active +blue+ +black+)
               :for record = (with-output-as-presentation (pane object 'node :single-box t)
                               (formatting-row (pane)
                                 (formatting-cell (pane :align-x :right)
                                   (princ (1+ (node-descendant-count object)) pane))
                                 (formatting-cell (pane)
                                   (with-text-face (pane face)
                                     (with-drawing-options (pane :ink ink)
                                       (princ (or (node-title object) "(Unknown)") pane))))))
               :when active :do (setf focus-record record))))))
    (when (and focus-record (not (region-intersects-region-p focus-record (pane-viewport-region pane))))
      (multiple-value-bind (x y) (output-record-position focus-record)
        (declare (ignore x))
        (scroll-extent pane 0 y)))))

(defun display-primary (frame pane)
  (let* ((selection (listener-selection frame))
         (text (and (messagep selection) (message-text selection))))
    (when text
      (stream-increment-cursor-position pane 12 6)
      (with-text-size (pane :small)
        (display-header selection pane))
      (with-output-as-presentation (pane selection 'message :single-box t)
        (with-text-style (pane +article-text-style+)
          (let ((lines (mapcar (lambda (line) (expand-tabs line 8))
                               (split-sequence #\newline
                                               (string-trim #.(concatenate 'string '(#\newline #\return #\tab #\space)) text)
                                               :remove-empty-subseqs nil))))
            (dolist (line lines)
              (terpri pane)
              (stream-increment-cursor-position pane 12 0)
              (with-drawing-options (pane :ink (if (scan "^\\s*([>|:]|<\\s).*" line) +gray40+ +black+))
                (let ((start 0))
                  (loop
                     (multiple-value-bind (mstart mend) (scan "\\b((?:https?|ftp)://[^/]+(?:/[a-zA-Z0-9!$%&/()=?*+~#_.:;-]*)?)" line :start start)
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
                             (setf start mend))))))))
            (fresh-line pane)
            (terpri pane)))))))

(defun display-current-thread (frame pane)
  (let* ((selection (listener-selection frame))
         (root (and selection (node-thread selection)))
         (memory (listener-memory frame))
         focus-record)
    (when memory
      (formatting-item-list (pane :x-spacing "WW")
        (dolist (item memory)
          (formatting-cell (pane)
            (with-output-as-presentation (pane item 'node :single-box t)
              (format pane "~A by ~A"
                      (or (node-title item) "(Unknown)")
                      (or (and (messagep item) (message-author item)) "(Unknown)"))))))
      (terpri pane))
    (labels
        ((collect-children (node)
           (let (children)
             (map-over-child-nodes (lambda (child) (push child children)) node)
             (nreverse children)))
         (paint-node (object stream)
           (let ((record
                  (with-output-as-presentation (stream object 'node :single-box t)
                    (with-text-face (stream (if (eql object selection) :bold :roman))
                      (with-drawing-options (stream :ink (if (eql object selection) +blue+ +black+))
                        (if (not (messagep object))
                            (format stream "~A" (node-title object))
                            (let ((date (message-date object))
                                  (author (message-author object))
                                  (title (node-title object)))
                              (when date
                                (setf date (format nil "~4,'0D-~2,'0D-~2,'0D"
                                                   (local-year date)
                                                   (local-month date)
                                                   (local-day date))))
                              (format stream "~A~@[ (~A)~]~@[: ~A~]"
                                      date author title))))))))
             (when (eql object selection)
               (setf focus-record record)))))
      (when root
        (with-text-size (pane :small)
          (format-hierarchy-from-roots (list root)
                                       #'collect-children
                                       :printer #'paint-node
                                       :line-style (make-line-style :thickness 1)
                                       :line-ink +gray85+
                                       :y-spacing 4
                                       :indentation-step 12
                                       :stream pane))
        (when focus-record
          (let ((vp-region (pane-viewport-region pane)))
            (unless (region-intersects-region-p vp-region focus-record)
              (multiple-value-bind (x y) (output-record-position focus-record)
                (declare (ignore x))
                (scroll-extent pane 0 y)))))))))
                      
              

