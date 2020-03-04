
(in-package "CLL-CLIM-UTILITIES")

(defparameter +weekday-symbols+ #(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))
(defparameter +weekday-abbrevs+ #("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"))
(defparameter +month-names+ #("" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))
(defparameter +month-abbrevs+ #("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar +acceptable-date-formatter+ (compile-timestamp-printer "yyyy-MM-dd"))
(defvar +acceptable-time-formatter+ (compile-timestamp-printer "HH:mm:ss.S"))
(defvar +acceptable-timestamp-formatter+ (compile-timestamp-printer "yyyy-MM-dd'T'HH:mm:ss.S"))

(define-presentation-type local-year-month ())
(define-presentation-type local-date ())
(define-presentation-type local-time ())
(define-presentation-type local-timestamp ())

(define-presentation-method present ((object local-date) (type local-date) stream view &key acceptably)
  (declare (ignore acceptably))
  (funcall +acceptable-date-formatter+ object :stream stream))

(define-presentation-method present ((object local-time) (type local-time) stream view &key acceptably)
  (declare (ignore acceptably))
  (funcall +acceptable-time-formatter+ object :stream stream))

(define-presentation-method present ((object local-timestamp) (type local-timestamp) stream view &key acceptably)
  (declare (ignore acceptably))
  (funcall +acceptable-timestamp-formatter+ object :stream stream))

(define-presentation-method present (object (type local-year-month) stream view &key acceptably)
  (declare (ignore acceptably))
  (format stream "~D-~2,'0D" (local-year object) (local-month object)))



(defun default-present-day (date stream)
  (let ((number (local-date-day date)))
    (princ number stream)))

(defun next-month (date)
  (let ((year (local-year date))
        (month (local-month date)))
    (if (eql month 12)
        (make-local-date (1+ year) 1 1)
        (make-local-date year (1+ month) 1))))

(defun previous-month (date)
  (let ((year (local-year date))
        (month (local-month date)))
    (if (eql month 1)
        (make-local-date (1- year) 12 1)
        (make-local-date year (1- month) 1))))

(defun plus-days (date count)
  (add-time-unit date count :day))

(defun current-local-date ()
  (multiple-value-bind (a b c d m y) (decode-universal-time (get-universal-time))
    (declare (ignore a b c))
    (make-local-date y m d)))

(defun display-calendar (year month
                         &key (stream *standard-output*) (printer nil)
                              (inside-ink +black+) (outside-ink +gray60+)
                              (first-weekday :monday) min-rows x-spacing
                              y-spacing)
  (let* ((month-start-date (make-local-date year month 1))
         (month-end-date (next-month month-start-date))
         (month-start-weekday (local-date-weekday month-start-date))
         (start-weekday-number (position first-weekday +weekday-symbols+))
         (weekday-labels +weekday-abbrevs+)
         (date (if (eql month-start-weekday start-weekday-number) month-start-date
                   (plus-days month-start-date
                              (* -1 (mod (+ month-start-weekday (- 14 start-weekday-number))
                                         7))))))
    (labels
        ((show-day (date stream)
           (if printer
               (funcall printer date stream)
               (let ((number (local-date-day date))
                     (color (if (and (local-date<= month-start-date date)
                                     (local-date< date month-end-date))
                                inside-ink outside-ink)))
                 (with-drawing-options (stream :ink color)
                   (princ number stream))))))
      (formatting-table (stream :x-spacing x-spacing :y-spacing y-spacing)
        (with-text-face (stream :bold)
          (formatting-row (stream)
            (loop
               for k upfrom 0 below 7
               for label = (aref weekday-labels (mod (+ start-weekday-number k) 7))
               do (formatting-cell (stream) (write-string label stream)))))
        (let ((rows 0))
          (loop
             (formatting-row (stream)
               (incf rows)
               (loop
                  repeat 7
                  do (formatting-cell (stream :align-x :right) (show-day date stream))
                     (setf date (plus-days date 1))))
             (when (local-date>= date month-end-date) (return)))
          (when min-rows
            (loop
               while (< rows min-rows)
               do (formatting-row (stream)
                    (incf rows)
                    (formatting-cell (stream :align-x :right)
                      (princ " " stream))))))))))

(define-presentation-type date-picker-command ())
(defparameter +min-local-date+ (make-local-date 1900 1 1))
(defparameter +max-local-date+ (make-local-date 2038 12 31))
(defparameter +first-date+ (make-local-date 2000 1 1))

(defun beginning-of-month (object)
  (make-local-date (local-year object) (local-month object) 1))

(defun menu-choose-local-date (&key
                                 initial-value x-position y-position
                                 (first-weekday :monday)
                                 associated-window
                                 (pointer-documentation *pointer-documentation-output*))
  (let* ((visible-start (beginning-of-month (or initial-value (current-local-date))))
         (visible-end (beginning-of-month (next-month visible-start))))
      (labels
          ((month-name (date)
             (aref +month-names+ (local-date-month date)))
           (paint-day (date stream)
             (let* ((inside (and (local-date<= visible-start date) (local-date<= date visible-end)))
                    (ink (if inside +black+ +gray60+))
                    (face (if (and initial-value (local-date= initial-value date)) :bold :roman)))
               (with-drawing-options (stream :ink ink)
                 (if (not inside)
                     (princ (local-date-day date) stream)
                     (with-output-as-presentation (stream date 'local-date :single-box t)
                       (with-text-face (stream face)
                         (princ (local-date-day date) stream)))))))
           (paint-calendar (stream)
             (surrounding-output-with-border (stream :shape :inset :padding-x 1 :padding-y 1)
               (surrounding-output-with-border (stream :shape :rectangle :filled t :ink +white+)
                 (display-calendar (local-date-year visible-start)
                                   (local-date-month visible-start)
                                   :stream stream :first-weekday first-weekday :min-rows 6
                                   :printer #'paint-day :x-spacing 6 :y-spacing 6))))
           (paint-caption (stream)
             (with-text-face (stream :bold)
               (format stream "~A ~D"
                       (string-capitalize (month-name visible-start))
                       (local-date-year visible-start))))
           (paint-button (command caption stream)
             (with-output-as-presentation (stream command 'date-picker-command)
               (write-symbol caption :stream stream :size :large)))
           (paint-all (stream)
             (with-room-for-graphics (stream :first-quadrant nil)
               (surrounding-output-with-border (stream :padding-x 8 :padding-y 8
                                                       :outline-ink +transparent-ink+)
                 (formatting-table (stream :y-spacing 12 :equalize-column-widths t)
                   (formatting-row (stream)
                     (formatting-cell (stream :align-x :center)
                       (paint-calendar stream)))
                   (formatting-row (stream)
                     (formatting-cell (stream :align-x :center)
                       (paint-caption stream)))
                   (formatting-row (stream)
                     (formatting-cell (stream :align-x :center)
                       (formatting-table (stream  :x-spacing 12)
                         (formatting-row (stream)
                           (formatting-cell (stream) (paint-button :prev-year :fast-backward stream))
                           (formatting-cell (stream) (paint-button :prev :step-backward stream))
                           (formatting-cell (stream) (paint-button :next :step-forward stream))
                           (formatting-cell (stream) (paint-button :next-year :fast-forward
                                                                   stream)))))))))))
        (with-menu (menu associated-window :label nil :scroll-bars nil)
          (paint-all menu)
          (climi::adjust-menu-size-and-position menu :x-position x-position :y-position y-position)
          (climi::enable-menu menu)
          (window-clear menu)
          (paint-all menu)
          (block done
            (let ((*pointer-documentation-output* pointer-documentation))
              (handler-case (loop
                               (multiple-value-bind (command argument event)
                                   (with-input-context (`(or local-date blank-area date-picker-command) :override t)
                                       (object type event)
                                       (prog1 nil (loop (read-gesture :stream menu)))
                                     (blank-area (values nil nil event))
                                     (date-picker-command (values :execute object nil))
                                     (local-date (values :return object event)))
                                 (ecase command
                                   ((nil) (return-from done (values nil nil)))
                                   ((:return) (return-from done (values argument event)))
                                   ((:execute)
                                    (let ((new-month (ecase argument
                                                       ((:prev)
                                                        (if (local-date= +min-local-date+ visible-start)
                                                            visible-start
                                                            (beginning-of-month (previous-month visible-start))))
                                                       ((:next)
                                                        (if (local-date= +max-local-date+ visible-end)
                                                            visible-start
                                                            (beginning-of-month (next-month visible-start))))
                                                       ((:prev-year)
                                                        (if (> (local-date-year visible-start) 1900)
                                                            (make-local-date (1- (local-date-year visible-start)) (local-date-month visible-start) 1)
                                                            visible-start))
                                                       ((:next-year)
                                                        (if (< (local-date-year visible-start) 2038) 
                                                            (make-local-date (1+ (local-date-year visible-start)) (local-date-month visible-start) 1)
                                                            visible-start)))))
                                      (unless (local-date= new-month visible-start)
                                        (setf visible-start new-month)
                                        (setf visible-end (beginning-of-month (next-month visible-start)))
                                        (window-clear menu)
                                        (paint-all menu)
                                        ))))))
                (abort-gesture ()
                  (return-from done (values nil nil))))))))))

