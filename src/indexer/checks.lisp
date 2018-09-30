
(in-package "CLL-INDEXER")

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
                 
