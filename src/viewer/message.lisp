
(in-package "CLL-VIEWER")

(defun messagep (object)
  (typep object 'message))

(defun thread-root-p (object)
  (typep object 'thread-root-message))

(defun section-root-p (object)
  (or (typep object 'orphans-node) (typep object 'threads-node)))



;;; Since the entire path from node back up to the root is (or at least
;;; may be) cached anyway, it is safe to store references to objects "above"
;;; in the tree in objects further "below". The inverse is not true: never
;;; store, say, a list of child nodes of some node A on A's property list!

(defgeneric node-thread (object)
  (:method ((object null)) nil)
  (:method ((object thread-root-message)) object)
  (:method ((object node))
    (multiple-value-bind (range found) (object-property object 'node-thread)
      (if found range
          (let ((value (node-thread (node-parent object))))
            (setf (object-property object 'node-thread) value)
            value)))))

(defgeneric node-section (object)
  (:method ((object null)) nil)
  (:method ((object threads-node)) object)
  (:method ((object orphans-node)) object)
  (:method ((object spam-node)) object)
  (:method ((object node))
    (multiple-value-bind (answer found) (object-property object 'node-section)
      (if found answer
          (let ((value (node-section (node-parent object))))
            (setf (object-property object 'node-section) value)
            value)))))

(defgeneric node-listener (object)
  (:method ((object node))
    (values (object-property (node-store object) 'listener))))

(defun node-successor (object)
  (cond*
    ((node-first-child object) => #'identity)
    ((node-next-sibling object) => #'identity)
    (t (loop
         :for upper := (node-parent object) :then (node-parent upper) :while upper
         :for sibling := (node-next-sibling upper)
         :when sibling
           :do (return sibling)))))

(defun node-predecessor (object)
  (let ((previous (node-previous-sibling object)))
    (if (not previous)
        (node-parent object)
        (loop
          (let ((last (node-last-child previous)))
            (if (null last) 
                (return previous)
                (setf previous last)))))))



(defclass date-range (annotatable)
  ((node
     :type node :initarg :node
     :reader date-range-node)
   (start
     :type integer :initarg :start
     :reader date-range-start)
   (end
     :type integer :initarg :end
     :reader date-range-end)
   (count
     :type integer :initarg :count
     :reader date-range-count)))

(defmethod node-listener ((object date-range))
  (node-listener (date-range-node object)))

(defmethod print-object ((object date-range) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (multiple-value-bind (sec min hour day month year) (decode-universal-time (date-range-start object) 0)
      (declare (ignore sec min hour day))
      (format stream "~4,'0D-~A OF ~S"
              year 
              (aref '#("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)
              (date-range-node object)))))


(defun collect-date-ranges (node)
  (loop
    :for (year month count) :in (collect-node-date-ranges node)
    :for start := (encode-universal-time 0 0 0 1 month year 0)
    :collect (multiple-value-bind (end-year end-month)
                 (if (= 12 month) (values (1+ year) 1) (values year (1+ month)))
               (make-instance 'date-range
                              :node node :start start 
                              :end (encode-universal-time 0 0 0 1 end-month end-year 0)
                              :count count))))

(defun date-range-threads (range)
  (let (result)
    (map-over-child-nodes (lambda (object)
                            (setf (object-property object 'thread-date-range) range)
                            (setf (object-property object 'thread-root) object)
                            (push object result))
                          (date-range-node range)
                          :from-end t 
                          :start-date (date-range-start range)
                          :end-date (date-range-end range))
    result))


(defun section-root-date-range-list (object) 
  (multiple-value-bind (list found) (object-property object 'date-range-list)
    (if found
        list
        (let ((list (collect-date-ranges object)))
          (setf (object-property object 'date-range-list) list)
          list))))


(defgeneric section-date-range-list (object)
  (:method ((object node)) (section-date-range-list (node-section object)))
  (:method ((object null)) nil)
  (:method ((object threads-node)) (section-root-date-range-list object))
  (:method ((object orphans-node)) (section-root-date-range-list object))
  (:method ((object spam-node)) (section-root-date-range-list object)))


(defgeneric node-section-date-range (object)
  (:method ((object null)) nil)
  (:method ((object date-range)) object)
  (:method ((object node))
    (multiple-value-bind (range found) (object-property object 'node-section-date-range)
      (cond 
        (found range)
        ((not (messagep object)) nil)
        (t (let* ((thread (node-thread object))
                  (ranges (section-date-range-list thread))
                  (date (message-date object))
                  (answer (find-if (lambda (range)
                                     (and (<= (date-range-start range) date)
                                          (< date (date-range-end range))))
                                   ranges)))
             (setf (object-property object 'node-section-date-range) answer)
             answer))))))



(define-presentation-type node ())

(define-presentation-method present ((object node) (type node) stream view &key acceptably)
  (if acceptably
      (format stream "#~S" (node-key object))
      (format stream "~A" (or (node-title object) (node-key object)))))

(define-presentation-method present ((object message) (type node) stream view &key acceptably)
  (if acceptably
      (call-next-method)
      (format stream "<~A>" (msgid-string (message-identifier object)))))

