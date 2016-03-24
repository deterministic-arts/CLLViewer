
(in-package "CLL-INDEXER")

#|
Some facts about the input file... 

 - Lines are terminated with plain newlines.
 - The encoding is apparently UTF-8.
 - Entries start with a line consisting only of "From ..." 
 - Headers don't seem to be ASCII-clean.
 - Each entry has a "Lines: xxx" header giving the length of the body in lines
 - Email addresses cannot be trusted to be well-formed. We don't need those anyway.
 - Message-IDs seem to be mostly clean (syntax-wise) but contain duplicates. As 
   far as I can tell, these duplicates are legitimate (i.e., the messages are 
   duplicates, not only their IDs)
 - Reference lists seem syntactially broken from time to time. It's somewhat
   horrible what you can find there.
|#



;;; We read the file as binary file. The only reason is, that we want
;;; to know the proper byte offsets of each message in the file, and 
;;; opening in character mode may get file-position to report "arbitrary"
;;; values instead of byte offsets.
;;;
;;; This is uncritical, since we already know the file to be valid
;;; UTF-8, and we read the file one line at a time (with #x0A as line
;;; terminator). No valid UTF-8 sequence can ever cross line boundaries. 

(defun read-ascii-line (buffer stream)
  "Read a single logical line from binary stream `stream' into `buffer', which
   must be a vector of element type `(unsigned-byte 8)' or compatible, a fill
   pointer and be adjustable. The result includes the terminating newline byte,
   i.e., #x0A if (and only if) the line was terminated by a newline. The newline
   will be missing, if the end of the input stream is reached."
  (setf (fill-pointer buffer) 0)
  (loop
    (let ((byte (read-byte stream nil nil)))
      (unless byte (return t))
      (vector-push-extend byte buffer)
      (when (eql byte 10) (return nil)))))


;;; The correct way to parse the file would be to evaluate the "Lines: xxx"
;;; header of each message. But this would not only be correct, but also require
;;; some real work. Just splitting at `From ...' boundaries seems to work fine.
;;; Let's add the hair, when it is really required.

(defun message-separator-p (buffer)
  (and (>= (length buffer) 8)
       (eql (aref buffer 0) #.(char-code #\F))
       (eql (aref buffer 1) #.(char-code #\r))
       (eql (aref buffer 2) #.(char-code #\o))
       (eql (aref buffer 3) #.(char-code #\m))
       (eql (aref buffer 4) #.(char-code #\space))
       (eql (aref buffer 5) #.(char-code #\.))
       (eql (aref buffer 6) #.(char-code #\.))
       (eql (aref buffer 7) #.(char-code #\.))
       (or (eql (length buffer) 8)
           (and (eql (length buffer) 9)
                (eql (aref buffer 8) #x0A)))))


(defun map-over-file-message-blobs (function pathname)
  (let ((line (make-array 128 :element-type 'octet :adjustable t :fill-pointer 0))
        (message (make-array 1024 :element-type 'octet :adjustable t :fill-pointer 0))
        (start 0)
        (offset 0))
    (labels
        ((flush ()
           (when (plusp (length message))
             (funcall function start (babel:octets-to-string message :encoding :utf-8))
             (setf (fill-pointer message) 0)))
         (grow ()
           (loop
             :for octet :across line 
             :do (vector-push-extend octet message))))
      (with-open-file (stream pathname
                              :direction :input
                              :element-type 'octet)
        (loop
          (let* ((eof (read-ascii-line line stream))
                 (read (length line))
                 (after (+ offset read)))
            (if (message-separator-p line)
                (progn
                  (flush)
                  (setf offset after)
                  (setf start after))
                (progn
                  (grow)
                  (setf offset after)))
            (when eof
              (flush)
              (return))))))))


(defun split-message (string 
                      &key (start 0) end)
  (let* ((string (string string))
         (end (or end (length string)))
         (headers nil)
         (scan-pointer start)
         (last-header nil))
    (labels
        ((whitespacep (char) (position char #.(concatenate 'string '(#\newline #\return #\space #\tab))))
         (separatorp (line) (and line (zerop (length line))))
         (continuationp (line) (and line (plusp (length line)) (whitespacep (char line 0))))
         (trim (string) (string-trim #.(concatenate 'string '(#\newline #\return #\space #\tab)) string))
         (rtrim (string) (string-right-trim #.(concatenate 'string '(#\newline #\return #\space #\tab)) string))
         (finish (string) (values (nreverse headers) string)))
      (loop
        :while (< scan-pointer end)
        :do (let* ((next-linefeed (or (position #\newline string :start scan-pointer :end end) end))
                   (line (subseq string scan-pointer next-linefeed)))
              (cond
                ((separatorp line) 
                 (when last-header (push last-header headers) (setf last-header nil))
                 (return (finish (subseq string next-linefeed end))))
                ((continuationp line)
                 (when last-header (setf (cdr last-header) (concatenate 'string (cdr last-header) " " (trim line))))
                 (setf scan-pointer (1+ next-linefeed)))
                (t (multiple-value-bind (match groups) (scan-to-strings "^([^\\s:]+)\\s*:\\s*(.*)$" string :start scan-pointer :end next-linefeed)
                     (unless match (error "unexpected line ~S" line))
                     (when last-header (push last-header headers) (setf last-header nil))
                     (setf last-header (cons (string-upcase (aref groups 0)) (rtrim (aref groups 1))))
                     (setf scan-pointer (1+ next-linefeed))))))
        :finally (when last-header (push last-header headers))
                 (return (finish ""))))))


(defun read-stream-message-blob (offset stream)
  (handler-case (file-position stream offset)
    (error (condition) (return-from read-stream-message-blob (values nil condition))))
  (let ((line (make-array 128 :element-type 'octet :adjustable t :fill-pointer 0))
        (message (make-array 1024 :element-type 'octet :adjustable t :fill-pointer 0)))
    (loop
      (let ((eof (read-ascii-line line stream)))
        (when (message-separator-p line) (return))
        (loop :for byte :across line :do (vector-push-extend byte message))
        (when eof (return))))
    (values (babel:octets-to-string message :encoding :utf-8)
            nil)))


(defun read-file-message-blob (offset pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :element-type 'octet)
    (read-stream-message-blob offset stream)))


(defmacro do-file-message-blobs ((offset blob) filename &body body)
  `(map-over-file-message-blobs (lambda (,offset ,blob) ,@body) ,filename))



(defun generate-message-index (source-file 
                               &optional (index-file (make-pathname :defaults source-file :type "cdb"))
                               &aux (temp-file (make-pathname :defaults source-file :type "cdb-temp")))
  (zcdb:with-output-to-cdb (cdb index-file temp-file) 
    (do-file-message-blobs (offset blob) source-file
      (multiple-value-bind (headers) (split-message blob)
        (let ((msgid-1 (make-msgid (format nil "msg.~D.cll.txt@deterministic-arts.de" offset)))
              (msgid-2 (let ((header (cdr (assoc "MESSAGE-ID" headers :test #'string-equal))))
                         (and header (parse-msgid header)))))
          (let ((key-1 (babel:string-to-octets (msgid-string msgid-1) :encoding :utf-8))
                (key-2 (and msgid-2 (babel:string-to-octets (msgid-string msgid-2) :encoding :utf-8)))
                (value (let ((array (make-array 4 :element-type 'octet)))
                         (loop
                           :for i :upfrom 0 :below 4
                           :for k :downfrom 24 :to 0 :by 8
                           :do (setf (aref array i) (ldb (byte 8 k) offset)))
                         array)))
            (zcdb:add-record key-1 value cdb)
            (when key-2
              (zcdb:add-record key-2 value cdb))))))))


(defun load-message-blob (key
                          &key (source-file #P"./cll.txt") 
                               (index-file (make-pathname :defaults source-file :type "cdb")))
  (let ((offset (if (typep key '(unsigned-byte 32)) key
                    (let* ((string (cond
                                    ((msgidp key) (msgid-string key))
                                    ((stringp key) key)
                                    ((symbolp key) (symbol-name key))
                                    ((characterp key) (string key))
                                    (t (error 'simple-type-error
                                              :datum key :expected-type '(or (unsigned-byte 32) msgid)
                                              :format-control "~S is not a well-formed message designator"
                                              :format-arguments (list key)))))
                           (array (zcdb:lookup (babel:string-to-octets string :encoding :utf-8) index-file)))
                      (and array (loop
                                   :with value := 0
                                   :for k :upfrom 0 :below 4
                                   :do (setf value (logior (ash value 8) (aref array k)))
                                   :finally (return value)))))))
    (when offset 
      (split-message (read-file-message-blob offset source-file)))))



(defconstant +root-id+ #xFFFFFFFF)
(defconstant +orphan-id+ #xFFFFFFFE)
(defconstant +threads-id+ #xFFFFFFFD)

(defparameter *schema*
  '(
"CREATE TABLE author (
  id INTEGER NOT NULL,
  address TEXT NOT NULL,
  PRIMARY KEY (id),
  UNIQUE (address)
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

"INSERT INTO message (id, msgid, parent_id, date, author_id, virtual, subject) VALUES (4294967295, 'root.cll.txt@deterministic-arts.net', NULL, 0, NULL, 1, 'All messages');"
"INSERT INTO message (id, msgid, parent_id, date, author_id, virtual, subject) VALUES (4294967294, 'orphans.cll.txt@deterministic-arts.net', 4294967295, 0, NULL, 1, 'Orphans');"
"INSERT INTO message (id, msgid, parent_id, date, author_id, virtual, subject) VALUES (4294967293, 'threads.cll.txt@deterministic-arts.net', 4294967295, 0, NULL, 1, 'Threads');"
"CREATE INDEX message_by_parent_and_date ON message (parent_id, date);"
"CREATE INDEX message_by_tree_start ON message (tree_start, tree_end);"
))

(defun create-tables (connection)
  (with-transaction (transaction connection :read-only nil)
    (dolist (command *schema*)
      (with-prepared-statement (statement transaction command)
        (loop while (step-statement statement))))))


(defun update-child-counts (connection)
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
          :for (id . count) :across buffer
          :do (reset-statement stmt)
              (bind-parameter stmt 1 count)
              (bind-parameter stmt 2 id)
              (loop :while (step-statement stmt)))))))


(defun update-nested-sets (connection)
  (let ((nodes (make-hash-table :test 'eql))
        (counter 0))
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
                 (loop while (step-statement stmt)))
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
        (with-prepared-statement (stmt transaction "UPDATE message SET n_descendants = tree_end - tree_start - 1")
          (loop while (step-statement stmt)))))))


(defun generate-index-database-1 (connection source-file progress-stream)
  (let ((max-batch-size 1000)
        (author-table (make-hash-table :test 'equal))
        (msgid-table (make-hash-table :test 'equal))
        (msgid-mapping (make-hash-table :test #'msgid= :hash-function #'msgid-hash))
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
               
           (enqueue-message (offset blob)
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
               (unless known
                 (setf (gethash msgid msgid-mapping) offset)
                 (push (list* offset msgid subject date author refs) batch-entries)
                 (incf batch-size)
                 (when (>= batch-size max-batch-size) (flush-batch)))))
           (intern-author (string tnx)
             (when string
               (let ((present (gethash string author-table)))
                 (or present
                     (let ((id (incf author-counter)))
                       (with-prepared-statement (stmt tnx "INSERT INTO author (id, address) VALUES (?, ?)")
                         (bind-parameter stmt 1 id)
                         (bind-parameter stmt 2 string)
                         (loop while (step-statement stmt))
                         (setf (gethash string author-table) id)
                         id))))))
           (flush-batch ()
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
               (when progress-stream
                 (write-char #\. progress-stream)
                 (force-output progress-stream)
                 (when (zerop (mod (incf batch-count) 20))
                   (terpri progress-stream)))
               (setf batch-size 0)
               (setf batch-entries nil))))
        (when progress-stream 
          (format progress-stream "~&[Indexing]~%")
          (force-output progress-stream))
        (do-file-message-blobs (offset blob) source-file
          (enqueue-message offset blob))
        (flush-batch)
        (when progress-stream
          (format progress-stream "~&[Finalizing threads]~%")
          (force-output progress-stream))
        (setf batch-count 0)
        (labels
            ((flush-batch ()
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
                 (when progress-stream
                   (write-char #\. progress-stream)
                   (force-output progress-stream)
                   (when (zerop (mod (incf batch-count) 20))
                     (terpri progress-stream)))
                 (setf batch-size 0)
                 (setf batch-entries nil))))
          (loop
            :for (fixup . refs) :in unresolved-references
            :for parent := (or (some (lambda (key) (gethash key msgid-mapping)) refs) +orphan-id+)
            :do (push (cons fixup parent) batch-entries)
                (incf batch-size)
                (when (>= batch-size max-batch-size)
                  (flush-batch)))
          (flush-batch))))))


(defun generate-index-database (&key (source-file #P"./cll.txt") 
                                     (database-file (make-pathname :defaults source-file :type "db"))
                                     (progress-stream *standard-output*))
  (ignore-errors (delete-file database-file))
  (with-connection (connection database-file)
    (create-tables connection)
    (generate-index-database-1 connection source-file progress-stream)
    (when progress-stream
      (format progress-stream "~&[Update child counters]~%")
      (force-output progress-stream))
    (update-child-counts connection)
    (when progress-stream
      (format progress-stream "~&[Update nested sets]~%")
      (force-output progress-stream))
    (update-nested-sets connection)
    (when progress-stream
      (format progress-stream "~&[Done]~%")
      (force-output progress-stream))
    database-file))



(defun hex-inspect (&key (pathname #P"cll.txt")
                         (width 16) (start 0) (end (+ start (* 8 width)))
                         (unprintable-character #\space)
                         (output *standard-output*))
  "Dump a hex representation of parts of a file into the given output
   stream. The function reads the portion of the file starting at byte
   offset `start', up to (but not including) `end' in chunks of `width'
   bytes. For each such chunk, a hex representation is dumped to the
   output stream. Unprintable characters are replaced by the character
   supplied as `unprintable-character'."
  (with-open-file (stream pathname
                          :direction :input
                          :element-type 'octet)
    (file-position stream start)
    (let ((buffer (make-array width :element-type 'octet))
          (text (make-string (+ (* width 3) width 1)))
          (digits "0123456789abcdef"))
      (loop
        :for offset :upfrom start :below end :by width
        :for limit := (read-sequence buffer stream)
        :do (when (zerop limit) (return))
            (let ((wp 0))
              (macrolet ((push-char (char) `(setf (char text (prog1 wp (incf wp))) ,char)))
                (loop
                  :for k :upfrom 0 :below limit
                  :for b := (aref buffer k)
                  :do (push-char (char digits (ash b -4)))
                      (push-char (char digits (logand b #xF)))
                      (push-char #\space))
                (loop
                  :for k :upfrom limit :below width
                  :do (push-char #\-) 
                      (push-char #\-) 
                      (push-char #\space))
                (loop
                  :for k :upfrom 0 :below limit
                  :for b := (aref buffer k)
                  :do (push-char (if (<= 32 b 126) (code-char b) unprintable-character)))
                (loop
                  :for k :upfrom limit :below width
                  :do (push-char #\space))
                (push-char #\newline)))
            (fresh-line output)
            (write-string text output)
            (when (< limit width) (return)))))
  (terpri output)
  pathname)


(defun ensure-utf-8-clean (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :element-type 'octet)
    (loop
      :with expected := 0 :and utf-8-seqs := 0
      :for offset :upfrom 0
      :for byte := (read-byte stream nil nil) :while byte
      :do (case (logand byte #b11000000)
            ((#b00000000 #b01000000) 
             (when (plusp expected)
               (format t "~&~D: Non-continuation (~D still missing)~%" offset expected)
               (setf expected 0)))
            ((#b10000000)
             (if (zerop expected) 
                 (format t "~&~D: Unexpected continuation character~%" offset)
                 (decf expected)))
            (t 
             (when (plusp expected)
               (format t "~&~D: Unexpected extended leader (~D still missing)~%" 
                       offset expected))
             (incf utf-8-seqs)
             (cond
                 ((eql (logand byte #b11100000) #b11000000) (setf expected 1))
                 ((eql (logand byte #b11110000) #b11100000) (setf expected 2))
                 ((eql (logand byte #b11111000) #b11110000) (setf expected 3))
                 ((eql (logand byte #b11111100) #b11111000) (setf expected 4))
                 ((eql (logand byte #b11111110) #b11111100) (setf expected 5))
                 ((eql (logand byte #b11111111) #b11111110) (setf expected 6))
                 (t (format t "~&~D: Ups. What's that?~%" offset)
                    (setf expected 0)))))
      :finally (when (plusp expected)
                 (format t "~&EOF: ~D still missing~%" expected))
               (format t "~&A total of ~D UTF-seqs was found.~%" utf-8-seqs))))


(defun check-from-lines ()
  (let ((seen (make-hash-table :test 'equal))
        (*allow-unicode* t)
        (*allow-obsolete-syntax* t))
    (do-file-message-blobs (offset blob) #P"cll.txt"
      (declare (ignore offset))
      (multiple-value-bind (headers body) (split-message blob)
        (declare (ignore body))
        (let ((author (cdr (assoc "FROM" headers :test #'string-equal))))
          (when (and author (not (gethash author seen)))
            (setf (gethash author seen) t)
            (unless (ignore-errors (mailbox author))
              (format t "~&Oops: ~S not parsable...~%" author))))))))


(defun check-references ()
  (let ((seen (make-hash-table :test #'msgid= :hash-function #'msgid-hash))
        (utable (make-hash-table :test 'equal))
        (*allow-unicode* t)
        (*allow-obsolete-syntax* t)
        (undefined 0))
    (do-file-message-blobs (offset blob) #P"cll.txt"
      (multiple-value-bind (headers body) (split-message blob)
        (declare (ignore body))
        (let* ((identifier (cdr (assoc "MESSAGE-ID" headers :test #'string-equal)))
               (references (cdr (assoc "REFERENCES" headers :test #'string-equal)))
               (msgid (and identifier (parse-msgid identifier :table utable)))
               (refs (and references (parse-msgid-list references :table utable))))
          (when (and references (plusp (length references)) (not refs)) (format t "~8,'0X: References: ~S~%" offset references))
          (when (and identifier (plusp (length identifier)) (not msgid)) (format t "~8,'0X: Message-ID: ~S~%" offset identifier))
          (unless identifier (format t "~8,'0X: No identifier~%" offset))
          (when msgid
            (let ((desc (gethash msgid seen)))
              (cond
                ((null desc) (setf (gethash msgid seen) (list nil offset)))
                ((null (cdr desc)) (push offset (cdr desc)))
                (t (push offset (cdr desc))))))
          (dolist (msgid refs)
            (let ((desc (gethash msgid seen)))
              (cond
                ((null desc) (setf (gethash msgid seen) (list offset)))
                (t (push offset (car desc)))))))))
    (maphash (lambda (key value)
               (cond
                 ((null (cdr value)) (incf undefined) #-(and)(format t "~&~A: Undefined identifier~%" key))
                 ((cddr value) (format t "~&~A: Ambiguous identifier (~{~S~^ ~})" key (cdr value)))))
             seen)
    (format t "~&~D undefined message IDs~%" undefined)))
                 
