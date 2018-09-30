
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


(defun map-over-file-message-blobs (function pathname &key pass-file-size)
  (let ((line (make-array 128 :element-type 'octet :adjustable t :fill-pointer 0))
        (message (make-array 1024 :element-type 'octet :adjustable t :fill-pointer 0))
        (size nil)
        (start 0)
        (offset 0))
    (labels
        ((flush ()
           (when (plusp (length message))
             (apply function start (babel:octets-to-string message :encoding :utf-8)
                    (and size (list size))) 
             (setf (fill-pointer message) 0)))
         (grow ()
           (loop
             :for octet :across line 
             :do (vector-push-extend octet message))))
      (with-open-file (stream pathname
                              :direction :input
                              :element-type 'octet)
        (when pass-file-size (setf size (file-length stream))) 
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


(defmacro do-file-message-blobs ((offset blob &optional size) filename &body body)
  `(map-over-file-message-blobs (lambda (,offset ,blob ,@(when size (list size))) ,@body)
                                  ,filename 
                                  ,@(when size (list :pass-file-size t))))

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
