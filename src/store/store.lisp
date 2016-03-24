
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
     :type (integer 0) :initarg :date :initform 0
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

(defclass standard-store (connection annotatable)
  ((message-file
     :type pathname :initarg :message-file
     :reader message-file)
   (index
     :initform (make-hash-table :test 'eql :weakness :value)
     :reader store-index)))

(defclass thread-root (node) ())
(defclass managed-node (basic-message indexed-node child-node parent-node) ())
(defclass standard-message (managed-node annotatable) ())
(defclass thread-root-message (managed-node thread-root annotatable) ())

(defclass strut-node (managed-node annotatable) ())
(defclass root-node (strut-node) ())
(defclass orphans-node (strut-node) ())
(defclass threads-node (strut-node) ())

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


(defun decode-message-text (headers text)
  (let* ((cte (cdr (assoc "Content-Transfer-Encoding" headers :test #'string-equal)))
         (qpp (string-equal cte "quoted-printable")))
    (if (not qpp) text
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
            buffer)))))
                            


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
               (date (statement-column-value stmt 3))
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
                            ((#xFFFFFFFD) 'threads-node))
                          (case parent
                            ((#xFFFFFFFE #xFFFFFFFD) 'thread-root-message)
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
    
(defun msg-query (&key (where nil) (group-by nil) (order-by nil) (having nil)
                       (limit nil) (offset nil))
  (when (and offset (not limit))
    (error "when ~S is supplied, ~S needs to be given as well" :offset :limit))
  (unless (typep offset '(or null (unsigned-byte 32)))
    (error 'type-error :datum offset :expected-type '(or null (unsigned-byte 32))))
  (unless (typep limit '(or null (unsigned-byte 32)))
    (error 'type-error :datum limit :expected-type '(or null (unsigned-byte 32))))  
  (format nil "SELECT m.id, m.msgid, m.parent_id, m.date, a.address, m.n_children, m.subject, m.n_descendants, m.tree_start, m.tree_end~@
               FROM message m LEFT JOIN author a ON a.id = m.author_id~
               ~@[~%WHERE ~A~]~
               ~@[~%GROUP BY ~{~A~^, ~}~]~
               ~@[~%HAVING ~A~]~
               ~@[~%ORDER BY ~{~A~^, ~}~]~
               ~@[~%LIMIT ~D~]~@[ OFFSET ~D~]"
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


(defmethod map-over-child-nodes (function (object indexed-node) 
                                 &key offset limit from-end start-date end-date)
  (when (plusp (node-child-count object))
    (let ((query (msg-query :where (format nil "m.parent_id = ?~
                                                ~:[~; AND m.date >= ?~]~
                                                ~:[~; AND m.date < ?~]"
                                           start-date end-date)
                            :order-by (if (not from-end) '("m.date ASC" "m.id DESC") '("m.date DESC" "m.id ASC"))
                            :limit limit :offset (and limit offset))))
      (map-over-messages function (node-store object) query 
                         (nconc (list (node-key object))
                                (when start-date (list start-date))
                                (when end-date (list end-date)))))))

(defmethod count-descendant-nodes ((object indexed-node) &key start-date end-date)
  (count-nodes (node-store object)
               (format nil "m.tree_start >= ? AND m.tree_start < ?~
                            ~:[~;~%AND m.date >= ?~]~
                            ~:[~;~%AND m.date < ?~]"
                       start-date end-date)
               (list* (node-tree-range-start object)
                      (node-tree-range-end object)
                      (nconc (when start-date (list start-date))
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
  (let ((parent (node-parent-key object))
        (self (node-key object))
        (date (message-date object))
        (query (msg-query :where "m.parent_id = ? AND (m.date > ? OR (m.date = ? AND m.id < ?))"
                          :order-by '("m.date ASC" "m.id DESC")
                          :limit 1))
        result)
    (do-messages (msg (node-store object) query (list parent date date self))
      (setf result msg))
    result))

(defmethod node-previous-sibling ((object managed-node))
  (let ((parent (node-parent-key object))
        (date (message-date object))
        (self (node-key object))
        (query (msg-query :where "m.parent_id = ? AND (m.date < ? OR (m.date = ? AND m.id > ?))"
                          :order-by '("m.date DESC" "m.id ASC")
                          :limit 1))
        result)
    (do-messages (msg (node-store object) query (list parent date date self))
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


(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun collect-node-date-ranges (object)
  (let* ((store (node-store object))
         (query (format nil "SELECT strftime('%Y%m', (CASE WHEN m.date < ~D THEN 0 ELSE m.date - ~D END), 'unixepoch') AS month~
                           ~%, COUNT(*) AS n_msgs~
                           ~%FROM message m~
                           ~%WHERE m.tree_start >= ~D AND m.tree_end < ~D~
                           ~%GROUP BY month ORDER BY month ASC"
                        +unix-epoch+ +unix-epoch+
                        (node-tree-range-start object)
                        (node-tree-range-end object))))
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
          
               



#-(and)
(with-open-store (store #P"./cll.txt")
  (pprint (collect-node-date-ranges (store-root store))))
