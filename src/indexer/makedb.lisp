
(in-package "CLL-INDEXER")

(defconstant +root-id+ #xFFFFFFFF)
(defconstant +orphan-id+ #xFFFFFFFE)
(defconstant +threads-id+ #xFFFFFFFD)
(defconstant +spam-id+ #xFFFFFFFC)

(defparameter *schema*
  '(
"CREATE TABLE person (
  id INTEGER NOT NULL,
  name TEXT NOT NULL,
  PRIMARY KEY (id)
);"

"CREATE TABLE mailbox (
  id INTEGER NOT NULL,
  address TEXT NOT NULL,
  person_id INTEGER NOT NULL,
  PRIMARY KEY (id),
  UNIQUE (address)
);"

"CREATE TABLE author (
  id INTEGER NOT NULL,
  address TEXT NOT NULL,
  mailbox_id INTEGER NOT NULL,
  PRIMARY KEY (id),
  UNIQUE (address),
  FOREIGN KEY (mailbox_id) REFERENCES mailbox (id)
);"

"CREATE TABLE message (
  id INTEGER NOT NULL,
  msgid TEXT NOT NULL,
  parent_id INTEGER DEFAULT NULL,
  date INTEGER NOT NULL,
  author_id INTEGER DEFAULT NULL,
  virtual INTEGER NOT NULL DEFAULT 0,
  n_children NOT NULL DEFAULT 0,
  n_descendants NOT NULL DEFAULT 0,
  tree_start INTEGER NOT NULL DEFAULT 0,
  tree_end INTEGER NOT NULL DEFAULT 0,
  subject TEXT,
  PRIMARY KEY (id),
  UNIQUE (msgid),
  FOREIGN KEY (author_id) REFERENCES author (id),
  FOREIGN KEY (parent_id) REFERENCES message (id)
);"

"CREATE TABLE bookmark (
  message_id INTEGER NOT NULL,
  priority INTEGER NOT NULL DEFAULT 0,
  description TEXT,
  PRIMARY KEY (message_id),
  FOREIGN KEY (message_id) REFERENCES message (id)
);"

"INSERT INTO message (id, msgid, parent_id, date, author_id, virtual, subject) VALUES (4294967295, 'root.cll.txt@deterministic-arts.net', NULL, 0, NULL, 1, 'All messages');"
"INSERT INTO message (id, msgid, parent_id, date, author_id, virtual, subject) VALUES (4294967294, 'orphans.cll.txt@deterministic-arts.net', 4294967295, 0, NULL, 1, 'Orphans');"
"INSERT INTO message (id, msgid, parent_id, date, author_id, virtual, subject) VALUES (4294967293, 'threads.cll.txt@deterministic-arts.net', 4294967295, 0, NULL, 1, 'Threads');"
"INSERT INTO message (id, msgid, parent_id, date, author_id, virtual, subject) VALUES (4294967292, 'spam.cll.txt@deterministic-arts.net', 4294967295, 0, NULL, 1, 'Spam');"
"CREATE INDEX message_by_parent_and_date ON message (parent_id, date);"
"CREATE INDEX message_by_tree_start ON message (tree_start, tree_end);"
))


(defun normalize-host (string &key (start 0) end)
  (let* ((end (or end (length string)))
         (length (- end start)))
    (if (and (plusp length) (eql #\[ (char string start)) (eql #\] (char string (1- end))))
        (let ((address (or (neta:parse-ipv4-address string :start (1+ start) :end (1- end) :junk-allowed t)
                           (neta:parse-ipv6-address string :start (1+ start) :end (1- end) :junk-allowed t))))
          (and address (neta:address-string address :prefix #\[ :suffix #\])))
        (let ((host (neta:parse-host-name string :start start :end end :junk-allowed t)))
          (and host (neta:address-string host))))))

(defun normalize-email-address (address)
  (multiple-value-bind (local-part host display-name) (parse-rfc5322-mailbox address :allow-obsolete-syntax t)
    (let ((address (and local-part host
                        (let ((normal-host (normalize-host host)))
                          (and normal-host
                               (format nil "~A@~A"
                                       (escape-local-part local-part)
                                       normal-host))))))
      (if address
          (values address display-name)
          (values nil nil)))))

(defun create-tables (connection)
  (signal-progress :phase :begin :action :initialize-schema)
  (with-transaction (transaction connection :read-only nil)
    (let ((total (length *schema*))
          (count 0))
      (dolist (command *schema*)
        (with-prepared-statement (statement transaction command)
          (loop while (step-statement statement)))
        (signal-progress :phase :work :action :initialize-schema :completion (/ (incf count) total)))))
  (signal-progress :phase :end :action :initialize-schema :completion 1))


(defun update-child-counts (connection)
  (signal-progress :action :update-child-counts :phase :begin)
  (with-transaction (transaction connection :read-only nil)
    (let ((buffer (make-array 1024 :element-type 't :fill-pointer 0 :initial-element nil :adjustable t)))
      (with-prepared-statement (stmt transaction "SELECT parent_id, COUNT(*) AS nch FROM message GROUP BY parent_id HAVING nch > 0")
        (loop
          :while (step-statement stmt)
          :do (let ((id (statement-column-value stmt 0))
                    (cn (statement-column-value stmt 1)))
                (vector-push-extend (cons id cn) buffer))))
      (with-prepared-statement (stmt transaction "UPDATE message SET n_children = ? WHERE id = ?")
        (loop
          :with item := 0
          :for (id . count) :across buffer
          :do (reset-statement stmt)
              (bind-parameter stmt 1 count)
              (bind-parameter stmt 2 id)
              (loop :while (step-statement stmt))
              (when (zerop (mod (incf item) 100))
                (signal-progress :action :update-child-counts
                                 :phase :update
                                 :completion (/ item (length buffer))))))))
  (signal-progress :action :update-child-counts :phase :end :completion 1))


(defun update-nested-sets (connection)
  (let ((nodes (make-hash-table :test 'eql))
        (counter 0)
        (written 0))
    (labels
        ((node-id (object) (first object))
         (node-range (object) (second object))
         ((setf node-range) (value object) (setf (second object) value))
         (node-start (object) (car (node-range object)))
         (node-end (object) (cdr (node-range object)))
         (node-children (object) (cddr object))
         (make-node (id) (list id nil))
         (add-child (child object) (push child (cddr object)))
         (intern-node (id) 
           (or (gethash id nodes)
               (let ((node (make-node id)))
                 (setf (gethash id nodes) node)
                 node))))
      (signal-progress :action :update-nested-sets :phase :begin :message "Loading messages" :completion 0)
      (with-transaction (transaction connection :read-only nil)
        (with-prepared-statement (stmt transaction "SELECT id, parent_id FROM message ORDER BY date DESC, id ASC")
          (loop
            :while (step-statement stmt)
            :do (let* ((node (intern-node (statement-column-value stmt 0)))
                       (pid (statement-column-value stmt 1))
                       (parent (and pid (intern-node pid))))
                  (when parent (add-child node parent)))))
        (with-prepared-statement (stmt transaction "UPDATE message SET tree_start = ?, tree_end = ? WHERE id = ?")
          (labels
              ((update (id start end)
                 (reset-statement stmt)
                 (bind-parameter stmt 1 start)
                 (bind-parameter stmt 2 end)
                 (bind-parameter stmt 3 id)
                 (loop while (step-statement stmt))
                 (incf written)
                 (when (zerop (mod written 100))
                   (signal-progress :action :update-nested-sets :phase :update
                                    :completion (/ written counter))))
               (traverse (node)
                 (let ((start (incf counter)))
                   (mapc #'traverse (node-children node))
                   (let ((end (1+ counter)))
                     (setf (node-range node) (cons start end))
                     end))))
            (traverse (gethash #xFFFFFFFF nodes))
            (maphash (lambda (key value)
                       (declare (ignore key))
                       (update (node-id value)
                               (node-start value)
                               (node-end value)))
                     nodes)))
        (signal-progress :action :update-nested-sets :phase :update 
                         :message "Updating descendant counters"
                         :completion 0)
        (with-prepared-statement (stmt transaction "UPDATE message SET n_descendants = tree_end - tree_start - 1")
          (loop while (step-statement stmt)))))))

(defun generate-index-database-1 (connection source-file)
  (let ((max-batch-size 1000)
        (author-table (make-hash-table :test 'equal))
        (mailbox-table (make-hash-table :test 'equal))
        (msgid-table (make-hash-table :test 'equal))
        (msgid-mapping (make-hash-table :test #'msgid= :hash-function #'msgid-hash))
        (completion 0)
        (mailbox-counter 0)
        (author-counter 0)
        (batch-entries nil)
        (batch-size 0)
        (batch-count 0)
        (unresolved-references nil))
    (let ((root-id (make-msgid "root.cll.txt@deterministic-arts.net" :table msgid-table))
          (orphans-id (make-msgid "orphans.cll.txt@deterministic-arts.net" :table msgid-table))
          (threads-id (make-msgid "threads.cll.txt@deterministic-arts.net" :table msgid-table)))
      (setf (gethash root-id msgid-mapping) +root-id+)
      (setf (gethash orphans-id msgid-mapping) +orphan-id+)
      (setf (gethash threads-id msgid-mapping) +threads-id+)
      (labels
          ((ignorable-id-char-p (char)
             (and (position char #.(concatenate 'string "<>," '(#\space #\newline #\tab #\return)))
                  t))
           (parse-id-list (raw)
             (if (or (not raw) (every #'ignorable-id-char-p raw)) 
                 (list threads-id)
                 (let ((parsed (parse-msgid-list raw :table msgid-table :reversed t)))
                   (or parsed (list orphans-id)))))
           (parse-id (raw offset)
             (or (unless (or (not raw) (every #'ignorable-id-char-p raw)) (parse-msgid raw :table msgid-table))
                 (make-msgid (format nil "~D.cll.txt@deterministic-arts.net" offset))))
           (parse-date (text)
             (when (and text (plusp (length text)))
               (or (ignore-errors (parse-date-time text))
                   (match text
                     ((ppcre "^\\s*(\\d{4})\\s*/\\s*(\\d{1,2})\\s*/\\s*(\\d{1,2})\\s*$" year month day)
                      (let ((year (parse-integer year)) (month (parse-integer month)) (day (parse-integer day)))
                        (when (and (<= 1 month 12) (<= 1 day 31))
                          (encode-universal-time 0 0 12 day month year 0))))
                     (_ nil)))))
           
           (enqueue-message (offset blob total-size)
             (let* ((headers (split-message blob))
                    (raw-msgid (cdr (assoc "Message-ID" headers :test #'string-equal)))
                    (raw-references (cdr (assoc "References" headers :test #'string-equal)))
                    (raw-author (cdr (assoc "From" headers :test #'string-equal)))
                    (raw-subject (cdr (assoc "Subject" headers :test #'string-equal)))
                    (raw-date (cdr (assoc "Date" headers :test #'string-equal)))
                    (msgid (parse-id raw-msgid offset))
                    (refs (parse-id-list raw-references))
                    (author (and raw-author (string-trim #.(concatenate 'string '(#\newline #\return #\tab #\space)) raw-author)))
                    (subject (and raw-subject (string-trim #.(concatenate 'string '(#\newline #\return #\tab #\space)) raw-subject)))
                    (date (or (parse-date raw-date) 0))
                    (known (gethash msgid msgid-mapping)))
               (setf completion (/ offset total-size))
               (unless known
                 (setf (gethash msgid msgid-mapping) offset)
                 (push (list* offset msgid subject date author refs) batch-entries)
                 (incf batch-size)
                 (when (>= batch-size max-batch-size) (flush-batch)))))
           (intern-mailbox (string tnx)
             (when string
               (multiple-value-bind (mailbox name) (normalize-email-address string)
                 (multiple-value-bind (mailbox name) (if mailbox (values mailbox name) (values string string))
                   (let ((present (gethash mailbox mailbox-table)))
                     (or present
                         (let ((id (incf mailbox-counter)))
                           (with-prepared-statement (stmt tnx "INSERT INTO person (id, name) VALUES (?, ?)")
                             (bind-parameter stmt 1 id)
                             (bind-parameter stmt 2 (or name mailbox))
                             (loop while (step-statement stmt)))
                           (with-prepared-statement (stmt tnx "INSERT INTO mailbox (id, address, person_id) VALUES (?, ?, ?)")
                             (bind-parameter stmt 1 id)
                             (bind-parameter stmt 2 mailbox)
                             (bind-parameter stmt 3 id)
                             (loop while (step-statement stmt)))
                           (setf (gethash mailbox mailbox-table) id)
                           id)))))))
           (intern-author (string tnx)
             (when string
               (let ((present (gethash string author-table)))
                 (or present
                     (let ((mbox (intern-mailbox string tnx))
                           (id (incf author-counter)))
                       (with-prepared-statement (stmt tnx "INSERT INTO author (id, address, mailbox_id) VALUES (?, ?, ?)")
                         (bind-parameter stmt 1 id)
                         (bind-parameter stmt 2 string)
                         (bind-parameter stmt 3 mbox)
                         (loop while (step-statement stmt))
                         (setf (gethash string author-table) id)
                         id))))))
           (flush-batch (&optional last)
             (when (plusp batch-size)
               (with-transaction (tnx connection :read-only nil)
                 (dolist (entry batch-entries)
                   (destructuring-bind (offset msgid subject date author &rest refs) entry
                     (assert refs)
                     (let ((author (intern-author author tnx))
                           (parent (gethash (car refs) msgid-mapping)))
                       (with-prepared-statement (stmt tnx "INSERT INTO message (id, msgid, date, author_id, parent_id, subject) VALUES (?, ?, ?, ?, ?, ?)")
                         (bind-parameter stmt 1 offset)
                         (bind-parameter stmt 2 (msgid-string msgid))
                         (bind-parameter stmt 3 date)
                         (bind-parameter stmt 4 author)
                         (bind-parameter stmt 5 parent)
                         (bind-parameter stmt 6 subject)
                         (loop while (step-statement stmt)))
                       (unless parent
                         (push (cons offset refs) unresolved-references))))))
               (signal-progress :action :index :phase (if last :end :update) :completion (if last 1 completion))
               (setf batch-size 0)
               (setf batch-entries nil))))
        (do-file-message-blobs (offset blob size) source-file
          (enqueue-message offset blob size))
        (flush-batch t)
        (setf batch-count 0)
        (let ((total-fixups (length unresolved-references))
              (completed-fixups 0))
          (labels
              ((flush-batch (&optional last)
                 (when (plusp batch-size)
                   (with-transaction (tnx connection :read-only nil)
                     (with-prepared-statement (adopt tnx "UPDATE message SET parent_id = ? WHERE id = ?")
                       (dolist (entry batch-entries)
                         (let ((msg (car entry))
                               (pid (cdr entry)))
                           (assert (and msg pid))
                           (reset-statement adopt)
                           (bind-parameter adopt 2 msg)
                           (bind-parameter adopt 1 pid)
                           (loop while (step-statement adopt))))))
                   (incf completed-fixups batch-size)
                   (setf batch-size 0)
                   (setf batch-entries nil)
                   (signal-progress :action :fixup-references 
                                    :phase (if last :end :update)
                                    :completion (if last 1 (/ completed-fixups total-fixups))))))
            (loop
              :for (fixup . refs) :in unresolved-references
              :for parent := (or (some (lambda (key) (gethash key msgid-mapping)) refs) +orphan-id+)
              :do (push (cons fixup parent) batch-entries)
                  (incf batch-size)
                  (when (>= batch-size max-batch-size)
                    (flush-batch)))
            (flush-batch t)))))))


(defun generate-index-database (&key (source-file #P"./cll.txt") 
                                     (database-file (make-pathname :defaults source-file :type "db"))
                                     ((:signal-progress *signal-progress*) *signal-progress*))
  (let ((*progressing-operation* `(generate-index-database ,source-file ,database-file))) 
    (ignore-errors (delete-file database-file))
    (with-connection (connection database-file)
      (create-tables connection)
      (generate-index-database-1 connection source-file)
      (update-child-counts connection)
      (update-nested-sets connection)
      database-file)))


(defun update-message-counters (connection 
                                &key ((:signal-progress *signal-progress*) *signal-progress*)
                                     (children-only nil))
  (let ((*progressing-operation* `(update-message-counters ,connection)))
    (update-child-counts connection)
    (unless children-only (update-nested-sets connection))
    connection))


(defun generate-index-database* (&key (source-file #P"./cll.txt") 
                                      (database-file (make-pathname :defaults source-file :type "db")))
  (handler-bind ((progress (lambda (object) (format t "~&~A~%" object))))
    (generate-index-database :source-file source-file :database-file database-file
                             :signal-progress t)))

#-(and)
(defun normalize-email-addresses (&key (source-file #P"./cll.txt")
                                    (database-file (make-pathname :defaults source-file :type "db")))
  (let ((*progressing-operation* `(normalize-email-addresses ,source-file ,database-file))
        (table (make-hash-table :test 'equal)))
    (with-connection (connection database-file)
      (with-transaction (transaction connection :read-only nil)
        (with-prepared-statement (stmt transaction "SELECT id, address FROM author")
          (loop
            while (step-statement stmt)
            do (let* ((id (statement-column-value stmt 0))
                      (address (statement-column-value stmt 1)))
                 (multiple-value-bind (mailbox name) (normalize-email-address address)
                   (when mailbox
                     (let ((entry (gethash mailbox table)))
                       (cond
                         ((not entry) (setf (gethash mailbox table) (list name id)))
                         ((or (car entry) (not name)) (push id (cdr entry)))
                         (t (setf (gethash mailbox table) (list* name id (cdr entry)))))))))))
        (format t "~&Found ~D unique proper email addresses~%"
                (hash-table-count table))))))
