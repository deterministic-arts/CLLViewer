
(in-package "CLL-STORE")

(defclass node () ())
(defclass message () ())
(defclass store () ())

(defgeneric store-root (object))
(defgeneric node-store (object))
(defgeneric node-key (object))

(defgeneric message-identifier (object))
(defgeneric message-author (object))
(defgeneric message-date (object))
(defgeneric message-subject (object))
(defgeneric message-file (object))
(defgeneric message-offset (object))
(defgeneric message-text (object))
(defgeneric message-headers (object))

(defgeneric count-descendant-nodes (object &key start-date end-date)
  (:method ((object node) &key start-date end-date) (declare (ignore object start-date end-date)) 0))

(defgeneric find-node-1 (key store)
  (:argument-precedence-order store key)
  (:method (key (object store))
    (declare (ignore key object))
    nil))

(defgeneric map-over-child-nodes (function node &key)
  (:argument-precedence-order node function)
  (:method (function (object node) &key)
    (declare (ignore function object))
    nil))

(defun find-node (key store &optional (errorp t) default)
  (let ((store (node-store store)))
    (cond
      ((and (typep key 'node) (eq store (node-store key))) (values key t))
      (t (let ((object (and key (find-node-1 key store))))
           (cond
             (object (values object t))
             ((not errorp) (values default nil))
             (t (error "there is no node matching ~S in ~S" key store))))))))

(defgeneric node-title (object)
  (:method ((object message)) (message-subject object))
  (:method ((object node)) 
    (with-output-to-string (stream)
      (princ object stream))))

(defgeneric node-child-count (object)
  (:method ((object node)) (declare (ignore object)) 0))

(defgeneric node-descendant-count (object)
  (:method ((object node)) (declare (ignore object)) 0))

(defgeneric node-tree-range-start (object)
  (:method ((object node)) (declare (ignore object)) 0))

(defgeneric node-tree-end-start (object)
  (:method ((object node)) (declare (ignore object)) 0))

(defgeneric node-next-sibling-key (object)
  (:method ((object node)) (declare (ignore object)) nil))

(defgeneric node-previous-sibling-key (object)
  (:method ((object node)) (declare (ignore object)) nil))

(defgeneric node-first-child-key (object)
  (:method ((object node)) (declare (ignore object)) nil))

(defgeneric node-last-child-key (object)
  (:method ((object node)) (declare (ignore object)) nil))


(defgeneric node-parent-key (object)
  (:method ((object node)) (declare (ignore object)) nil))

(defgeneric node-parent (object)
  (:method ((object node))
    (let ((key (node-parent-key object)))
      (and key (find-node key object)))))

(defgeneric node-path (object)
  (:method ((object node))
    (let ((parent (node-parent object)))
      (if parent
          (cons object (node-path parent))
          (list object)))))


(defgeneric node-previous-sibling (object)
  (:method ((object node))
    (let ((key (node-previous-sibling-key object)))
      (and key (find-node key object)))))

(defgeneric node-next-sibling (object)
  (:method ((object node))
    (let ((key (node-next-sibling-key object)))
      (and key (find-node key object)))))

(defgeneric node-first-child (object)
  (:method ((object node))
    (let ((key (node-first-child-key object)))
      (and key (find-node key object)))))

(defgeneric node-last-child (object)
  (:method ((object node))
    (let ((key (node-last-child-key object)))
      (and key (find-node key object)))))



(defparameter +default-date+ (make-local-timestamp 1970 1 1 0 0 0))

(defclass basic-message (message)
  ((identifier
     :type msgid :initarg :identifier
     :reader message-identifier)
   (subject
     :type (or null string) :initarg :subject :initform nil
     :reader message-subject)
   (author 
     :type (or null string) :initarg :author :initform nil
     :reader message-author)
   (date
     :type local-timestamp :initarg :date :initform +default-date+
     :reader message-date)))

(defclass indexed-node (node)
  ((store
     :type store :initarg :store
     :reader node-store)
   (key
     :type (unsigned-byte 64) :initarg :key
     :reader node-key)))

(defclass child-node (node)
  ((parent-key
     :initarg :parent-key :initform nil
     :reader node-parent-key)
   (cached-parent
     :initform :unresolved 
     :accessor cached-parent)))

(defclass parent-node (node)
  ((child-count 
     :initarg :child-count :initform 0
     :reader node-child-count)
   (descendant-count
     :initarg :descendant-count :initform 0
     :reader node-descendant-count)
   (tree-range-start
     :initarg :tree-range-start :initform 0
     :reader node-tree-range-start)
   (tree-range-end 
     :initarg :tree-range-end :initform 0
     :reader node-tree-range-end)))

(defclass standard-store (connection property-support)
  ((message-file
     :type pathname :initarg :message-file
     :reader message-file)
   (index
     :initform (make-hash-table :test 'eql :weakness :value)
     :reader store-index)))

(defclass thread-root (node) ())
(defclass managed-node (basic-message indexed-node child-node parent-node) ())
(defclass standard-message (managed-node property-support) ())
(defclass thread-root-message (managed-node thread-root property-support) ())

(defclass strut-node (managed-node property-support) ())
(defclass root-node (strut-node) ())
(defclass orphans-node (strut-node) ())
(defclass threads-node (strut-node) ())
(defclass spam-node (strut-node) ())

(defmethod node-parent ((object child-node))
  (let ((cached (cached-parent object)))
    (if (eql cached :unresolved)
        (setf (cached-parent object) 
              (let ((key (node-parent-key object)))
                (and key (find-node key object))))
        cached)))

(defmethod message-file ((object managed-node))
  (message-file (node-store object)))

(defmethod message-offset ((object managed-node))
  (node-key object))

(defun flush-message-caches (store)
  (clrhash (store-index store))
  store)

(defun decode-quoted-printable-text (text)
  (let* ((end (length text))
         (buffer (make-array end :element-type 'character :adjustable t :fill-pointer 0))
         (start 0)
         (stop 0))
    (labels
        ((copy (start end)
           (when (< start end)
             (loop
               :for k :upfrom start :below end
               :do (vector-push-extend (char text k) buffer)))))
      (loop
        (cond 
          ((>= stop end) (copy start end) (return))
          ((not (eql (char text stop) #\=)) (incf stop))
          (t (copy start stop)
             (incf stop)
             (let ((avail (- end stop)))
               (if (not (plusp avail)) (return)
                   (cond
                     ((eql #\newline (char text stop)) (incf stop) (setf start stop))
                     ((< avail 2) (return))
                     (t (let ((digit1 (digit-char-p (char text stop) 16))
                              (digit2 (digit-char-p (char text (1+ stop)) 16)))
                          (when (and digit1 digit2)
                            (vector-push-extend (code-char (logior (ash digit1 4) digit2)) buffer))
                          (incf stop 2)
                          (setf start stop)))))))))
      buffer)))

(defun decode-base64-text (text &key (encoding :iso-8859-1))
  (let ((bytes (cl-base64:base64-string-to-usb8-array text)))
    (babel:octets-to-string bytes :encoding encoding)))


(defun decode-message-text (headers text)
  (let ((cte (cdr (assoc "Content-Transfer-Encoding" headers :test #'string-equal))))
    (cond
      ((equalp cte "QUOTED-PRINTABLE") (decode-quoted-printable-text text))
      ((equalp cte "BASE64") (decode-base64-text text))
      (t text))))


(defmethod message-text ((object message))
  (let* ((offset (message-offset object))
         (file (message-file object))
         (blob (read-file-message-blob offset file)))
    (when blob
      (multiple-value-bind (headers text) (split-message blob)
        (decode-message-text headers text)))))

(defmethod message-headers ((object message))
  (let* ((offset (message-offset object))
         (file (message-file object))
         (blob (read-file-message-blob offset file)))
    (and blob
         (nth-value 0 (split-message blob)))))



(defun open-store (blob-file &optional (database-file (make-pathname :defaults blob-file :type "db")))
  (unless (probe-file database-file)
    (error "file ~S does not exist" database-file))
  (let ((connection (open-connection database-file :class 'standard-store)))
    (setf (slot-value connection 'message-file) blob-file)
    connection))

(defmethod close-connection :after ((object standard-store))
  (clrhash (store-index object)))

(defun close-store (store)
  (close-connection store))

(defmethod node-store ((object transaction))
  (transaction-connection object))

(defmethod node-store ((object standard-store))
  object)

(defmacro with-open-store ((var pathname &optional (db-spec nil have-db-spec)) &body body)
  (let ((temp (gensym)))
    `(let (,temp)
       (unwind-protect (progn 
                         (setf ,temp (open-store ,pathname ,@(when have-db-spec (list db-spec))))
                         (let ((,var ,temp))
                           ,@body))
         (when ,temp
           (close-store ,temp))))))



(defun extract-message (store stmt)
  (let* ((offset (statement-column-value stmt 0))
         (table (store-index store))
         (object (gethash offset table)))
    (or object
        (let* ((identifier (make-msgid (statement-column-value stmt 1)))
               (parent (statement-column-value stmt 2))
               (date (local-timestamp (universal-time-to-instant (statement-column-value stmt 3))))
               (author (statement-column-value stmt 4))
               (n-children (statement-column-value stmt 5))
               (subject (statement-column-value stmt 6))
               (n-descendants (statement-column-value stmt 7))
               (tree-start (statement-column-value stmt 8))
               (tree-end (statement-column-value stmt 9))
               (class (if (>= offset #xFFFFFF00) 
                          (ecase offset
                            ((#xFFFFFFFF) 'root-node)
                            ((#xFFFFFFFE) 'orphans-node)
                            ((#xFFFFFFFD) 'threads-node)
                            ((#xFFFFFFFC) 'spam-node))
                          (case parent
                            ((#xFFFFFFFE #xFFFFFFFD #xFFFFFFFC) 'thread-root-message)
                            (otherwise 'standard-message)))))
          (let ((object (make-instance class
                                       :store store :key offset :identifier identifier
                                       :author (and author (decode-=???=-words author)) 
                                       :date date 
                                       :subject (and subject (decode-=???=-words subject)) 
                                       :child-count n-children :parent-key parent 
                                       :descendant-count n-descendants
                                       :tree-range-start tree-start :tree-range-end tree-end)))
            (setf (gethash offset table) object)
            object)))))

(defparameter +msg-projection+ "m.id, m.msgid, m.parent_id, m.date, a.address, m.n_children, m.subject, m.n_descendants, m.tree_start, m.tree_end")
(defparameter +msg-projection-len+ 10)
    
(defun msg-query (&key (where nil) (group-by nil) (order-by nil) (having nil)
                       (limit nil) (offset nil))
  (when (and offset (not limit))
    (error "when ~S is supplied, ~S needs to be given as well" :offset :limit))
  (unless (typep offset '(or null (unsigned-byte 32)))
    (error 'type-error :datum offset :expected-type '(or null (unsigned-byte 32))))
  (unless (typep limit '(or null (unsigned-byte 32)))
    (error 'type-error :datum limit :expected-type '(or null (unsigned-byte 32))))  
  (format nil "SELECT ~A~@
               FROM message m LEFT JOIN author a ON a.id = m.author_id~
               ~@[~%WHERE ~A~]~
               ~@[~%GROUP BY ~{~A~^, ~}~]~
               ~@[~%HAVING ~A~]~
               ~@[~%ORDER BY ~{~A~^, ~}~]~
               ~@[~%LIMIT ~D~]~@[ OFFSET ~D~]"
          +msg-projection+
          where group-by having order-by limit offset))


(defun count-nodes (store &optional filter parameters)
  (let ((query (format nil "SELECT COUNT(*) AS cnt~@
                            FROM message m LEFT JOIN author a ON a.id = m.author_id~
                            ~@[~%WHERE ~A~]"
                       filter)))
    (with-transaction (tnx store :read-only t)
      (with-prepared-statement (stmt tnx query)
        (loop
          :for value :in parameters
          :for index :upfrom 1
          :do (bind-parameter stmt index value))
        (let (result)
          (loop
            :while (step-statement stmt) 
            :do (setf result (statement-column-value stmt 0)))
          (or result 0))))))
          
(defun map-over-messages (function store query parameters)
  (let ((store (node-store store)))
    (with-transaction (tnx store :read-only t)
      (with-prepared-statement (stmt tnx query)
        (loop
          :for value :in parameters
          :for index :upfrom 1
          :do (bind-parameter stmt index value))
        (loop
          :while (step-statement stmt)
          :do (funcall function (extract-message store stmt)))))))

(defmacro do-messages ((var store query &optional (parameters 'nil)) &body body)
  (let ((temp (gensym "DO-MESSAGE-CONTINUATION")))
    `(flet ((,temp (,var) ,@body))
       (declare (dynamic-extent #',temp))
       (map-over-messages #',temp ,store ,query ,parameters))))

(defmethod find-node-1 (key (store standard-store))
  (cond*
    ((and (typep key 'node) (eq store (node-store key))) key)
    ((and (integerp key) (gethash key (store-index store))) => #'identity)
    (t (flet ((bad-key ()
                (error 'simple-type-error 
                           :datum key :expected-type '(or integer string msgid)
                           :format-control "~S is not a supported node designator for ~S"
                           :format-arguments (list key store))))
         (multiple-value-bind (criterion parameters) 
             (cond
               ((member key '(t :root)) (values "m.id = ?" (list #xFFFFFFFF)))
               ((eql key :orphans) (values "m.id = ?" (list #xFFFFFFFE)))
               ((eql key :threads) (values "m.id = ?" (list #xFFFFFFFD)))
               ((eql key :spam) (values "m.id = ?" (list #xFFFFFFFC)))
               ((integerp key) (values "m.id = ?" (list key)))
               ((msgidp key) (values "m.msgid = ?" (list (msgid-string key))))
               ((stringp key) (values "m.msgid = ?" (list key)))
               ((typep key 'node)
                (let ((key (node-key key)))
                  (if (integerp key) 
                      (values "m.id = ?" (list key))
                      (bad-key))))
               (t (bad-key)))
           (let (result)
             (do-messages (object store (msg-query :where criterion) parameters)
               (setf result object))
             result))))))


(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun to-universal (value)
  (etypecase value
    (local-timestamp (instant-to-universal-time (instant value)))
    (local-date (instant-to-universal-time (instant value)))
    (null nil)
    (integer value)))


(defmethod map-over-child-nodes (function (object indexed-node) 
                                 &key offset limit from-end
                                   ((:start-date sd) nil)
                                   ((:end-date ed) nil)
                                 &aux
                                   (start-date (to-universal sd))
                                   (end-date (to-universal ed))
                                   (use-start (and start-date (> start-date +unix-epoch+))))
  (when (plusp (node-child-count object))
    (let ((query (msg-query :where (format nil "m.parent_id = ?~
                                                ~:[~; AND m.date >= ?~]~
                                                ~:[~; AND m.date < ?~]"
                                           (and use-start start-date)
                                           end-date)
                            :order-by (if (not from-end) '("m.date ASC" "m.id DESC") '("m.date DESC" "m.id ASC"))
                            :limit limit :offset (and limit offset))))
      (map-over-messages function (node-store object) query 
                         (nconc (list (node-key object))
                                (when use-start (list start-date))
                                (when end-date (list end-date)))))))

(defmethod count-descendant-nodes ((object indexed-node) 
                                   &key ((:start-date sd) nil) ((:end-date ed) nil)
                                   &aux
                                     (start-date (to-universal sd))
                                     (end-date (to-universal ed))
                                     (use-start (and start-date (> start-date +unix-epoch+))))
                                                         
  (count-nodes (node-store object)
               (format nil "m.tree_start >= ? AND m.tree_start < ?~
                            ~:[~;~%AND m.date >= ?~]~
                            ~:[~;~%AND m.date < ?~]"
                       (and use-start start-date) end-date)
               (list* (node-tree-range-start object)
                      (node-tree-range-end object)
                      (nconc (when use-start (list start-date))
                             (when end-date (list end-date))))))
  

(defmethod store-root ((object standard-store))
  (find-node #xFFFFFFFF object))

(defmethod node-first-child ((object indexed-node))
  (let (result)
    (map-over-child-nodes (lambda (child) (setf result child)) object :limit 1 :offset 0)
    result))

(defmethod node-last-child ((object indexed-node))
  (let (result)
    (map-over-child-nodes (lambda (child) (setf result child)) object :limit 1 :offset 0 :from-end t)
    result))

(defmethod node-next-sibling ((object managed-node))
  (let* ((parent (node-parent-key object))
         (self (node-key object))
         (date (message-date object))
         (u-date (to-universal date))
         (query (msg-query :where "m.parent_id = ? AND (m.date > ? OR (m.date = ? AND m.id < ?))"
                           :order-by '("m.date ASC" "m.id DESC")
                           :limit 1))
         result)
    (do-messages (msg (node-store object) query (list parent u-date u-date self))
      (setf result msg))
    result))

(defmethod node-previous-sibling ((object managed-node))
  (let* ((parent (node-parent-key object))
         (date (message-date object))
         (u-date (to-universal date))
         (self (node-key object))
         (query (msg-query :where "m.parent_id = ? AND (m.date < ? OR (m.date = ? AND m.id > ?))"
                           :order-by '("m.date DESC" "m.id ASC")
                           :limit 1))
         result)
    (do-messages (msg (node-store object) query (list parent u-date u-date self))
      (setf result msg))
    result))

(defmethod node-next-sibling-key ((object managed-node))
  (let ((node (node-next-sibling object)))
    (and node (node-key node))))

(defmethod node-previous-sibling-key ((object managed-node))
  (let ((node (node-previous-sibling object)))
    (and node (node-key node))))

(defmethod node-first-child-key ((object indexed-node))
  (let ((child (node-first-child object)))
    (and child (node-key child))))

(defmethod node-last-child-key ((object indexed-node))
  (let ((child (node-last-child object)))
    (and child (node-key child))))


(defun collect-node-date-ranges (object)
  (let* ((store (node-store object))
         (query (format nil "SELECT strftime('%Y%m', (CASE WHEN m.date < ~D THEN 0 ELSE m.date - ~D END), 'unixepoch') AS month~
                           ~%, COUNT(*) AS n_msgs~
                           ~%FROM message m~
                           ~%WHERE m.parent_id = ~D~
                           ~%GROUP BY month ORDER BY month ASC"
                        +unix-epoch+ +unix-epoch+
                        (node-key object))))
    (with-transaction (tnx store :read-only t)
      (with-prepared-statement (stmt tnx query)
        (loop
          :for ok := (step-statement stmt) :while ok
          :for key := (and ok (statement-column-value stmt 0))
          :for count := (and ok (statement-column-value stmt 1))
          :when ok
            :collect (let ((year (parse-integer key :start 0 :end 4))
                           (month (parse-integer key :start 4)))
                       (list year month count)))))))
          


(defun reparent-node (child parent)
  (let ((store (node-store child)))
    (unless (eql store (node-store parent))
      (error "cannot reparent nodes across stores"))
    (with-transaction (tnx store :read-only nil)
      (let ((child-path (node-path child))
            (parent-path (node-path parent)))
        (cond
          ((member child parent-path) (error "child is contained in new parent's path"))
          ((member parent child-path) nil)
          (t (with-prepared-statement (stmt tnx "UPDATE message SET parent_id = ? WHERE id = ?")
               (bind-parameter stmt 1 (node-key parent))
               (bind-parameter stmt 2 (node-key child))
               (loop while (step-statement stmt))
               (flush-message-caches store)
               child)))))))



(defgeneric bookmark-node (object))
(defgeneric bookmark-priority (object))
(defgeneric bookmark-description (object))

(defclass bookmark () 
  ((node
     :type node :initarg :node
     :reader bookmark-node)
   (priority
     :type integer :initarg :priority
     :reader bookmark-priority)
   (description
     :type string :initarg :description
     :reader bookmark-description)))

(defmethod node-store ((object bookmark))
  (node-store (bookmark-node object)))

(defmethod bookmark-node ((object node))
  object)


(defun map-over-bookmarks (function store 
                           &key from-end limit offset)
  (let ((store (node-store store))
        (query (format nil "SELECT ~A, b.priority, b.description~
                          ~%FROM (message m~
                          ~%INNER JOIN bookmark b ON b.message_id = m.id)~
                          ~%LEFT JOIN author a ON m.author_id = a.id~
                          ~%ORDER BY ~A~
                          ~@[~%LIMIT ~D~]~
                          ~@[~%OFFSET ~D~]"
                       +msg-projection+
                       (if from-end
                           "b.priority ASC, b.message_id DESC"
                           "b.priority DESC, b.message_id ASC")
                       limit (and limit offset))))
    (with-transaction (tnx store)
      (with-prepared-statement (stmt tnx query)
        (loop
          :while (step-statement stmt)
          :do (let ((msg (extract-message store stmt))
                    (priority (statement-column-value stmt (+ +msg-projection-len+ 0)))
                    (description (statement-column-value stmt (+ +msg-projection-len+ 1))))
                (funcall function (make-instance 'bookmark
                                                 :node msg :priority priority
                                                 :description description))))))))


(defun list-bookmarks (store)
  (let (result)
    (map-over-bookmarks (lambda (object) (push object result)) store :from-end t)
    result))

(defun find-bookmark-1 (designator store transaction)
  (multiple-value-bind (criterion parameters)
      (typecase designator
        (bookmark (values "m.id = ?" (list (node-key (bookmark-node designator)))))
        (indexed-node (values "m.id = ?" (list (node-key designator))))
        (integer (values "m.id = ?" (list designator)))
        (msgid (values "m.msgid = ?" (list (msgid-string designator))))
        (string (values "m.msgid = ?" (list designator)))
        ((eql :orphans) (values "m.id = ?" (list #xFFFFFFFE)))
        ((eql :threads) (values "m.id = ?" (list #xFFFFFFFD)))
        ((eql :threads) (values "m.id = ?" (list #xFFFFFFFC)))
        ((eql :root) (values "m.id = ?" (list #xFFFFFFFF)))
        (t (error 'simple-type-error
                  :datum designator :expected-type '(or integer msgid node)
                  :format-control "~S is not a well-formed node designator"
                  :format-arguments (list designator))))
    (let ((result nil)
          (query (format nil "SELECT ~A, b.priority, b.description~
                            ~%FROM message m INNER JOIN author a ON m.author_id = a.id~
                            ~%INNER JOIN bookmark b ON b.message_id = m.id~
                            ~%WHERE ~A" +msg-projection+ criterion)))
      (with-prepared-statement (stmt transaction query)
        (loop
          :for index :upfrom 1 
          :for value :in parameters 
          :do (bind-parameter stmt index value))
        (loop
          :while (step-statement stmt)
          :do (let* ((node (extract-message store stmt))
                     (priority (statement-column-value stmt (+ +msg-projection-len+ 0)))
                     (description (statement-column-value stmt (+ +msg-projection-len+ 1)))
                     (bookmark (make-instance 'bookmark
                                              :node node :priority priority
                                              :description description)))
                (setf result bookmark)))
        result))))


(defun find-bookmark (designator store)
  (let ((store (node-store store)))
    (with-transaction (tnx store)
      (find-bookmark-1 designator store tnx))))


(defun update-bookmark* (node properties)
  (let ((node (bookmark-node node)))
    (destructuring-bind (&key (priority 0 have-priority) (description nil have-description)) properties
      (let ((store (node-store node)))
        (with-transaction (tnx store :read-only nil)
          (let* ((node (find-node node store))
                 (present (find-bookmark-1 (node-key node) store tnx)))
            (if present
                (let* ((old-priority (bookmark-priority present))
                       (old-description (bookmark-description present))
                       (same-priority (or (not have-priority) (eql priority old-priority)))
                       (same-description (or (not have-description) (equal description old-description))))
                  (unless (and same-priority same-description)
                    (with-prepared-statement (stmt tnx "UPDATE bookmark SET priority = ?, description = ? WHERE message_id = ?")
                      (bind-parameter stmt 1 priority)
                      (bind-parameter stmt 2 description)
                      (bind-parameter stmt 3 (node-key (bookmark-node present)))
                      (loop while (step-statement stmt))))
                  (setf (slot-value present 'priority) priority)
                  (setf (slot-value present 'description) description)
                  present)
                (let* ((priority (etypecase priority ((signed-byte 64) priority)))
                       (description (and description (string description)))
                       (object (make-instance 'bookmark
                                              :node node :priority priority
                                              :description description)))
                  (with-prepared-statement (stmt tnx "INSERT INTO bookmark (message_id, priority, description) VALUES (?, ?, ?)")
                    (bind-parameter stmt 1 (node-key node))
                    (bind-parameter stmt 2 priority)
                    (bind-parameter stmt 3 description)
                    (loop while (step-statement stmt)))
                  object))))))))
                

(defun update-bookmark (node &rest properties &key (priority 0) description)
  (declare (ignore priority description))
  (update-bookmark* node properties))

  
(defun delete-bookmark (object)
  (let* ((node (bookmark-node object))
         (store (node-store node)))
    (with-transaction (tnx store :read-only nil)
      (with-prepared-statement (stmt tnx "DELETE FROM bookmark WHERE message_id = ?")
        (bind-parameter stmt 1 (node-key node))
        (loop while (step-statement stmt))))))
