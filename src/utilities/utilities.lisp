
(in-package "CLL-UTILS")


(defmacro cond* (&body clauses)
  (if (null clauses) nil
      (let ((rev-clauses (reverse clauses)))
        (loop
          :with seed := 'nil
          :for clause :in rev-clauses
          :do (destructuring-bind (test &rest body) clause
                (setf seed 
                      (if (and (eql (length body) 2) (symbolp (car body)) (string= (car body) "=>"))
                          (let ((temp (gensym)))
                            `(let ((,temp ,test))
                               (if ,temp (funcall ,(second body) ,temp) ,seed)))
                          `(if ,test (progn ,@body) ,seed))))
          :finally (return seed)))))




(defparameter +normalized-encodings+
  '((:utf-8 "UTF-8" "UTF8")
    (:iso-8859-1 "ISO-8859-1" "LATIN1" "LATIN-1")
    (:cp1252 "WINDOWS-1252" "CP1252")))
  

(defun decode-=???=-words (text &key (start 0) end)
  (declare (optimize speed))
  (let* ((string (string text))
         (end (or end (length string)))
         (byte-array nil))
    (labels
        ((get-byte-array ()
           (or byte-array
               (setf byte-array (make-array (- end start) :element-type '(unsigned-byte 8) :fill-pointer 0))))
         
         (normalize-encoding (string start end)
           (block found
             (loop
               :for (encoding . keys) :in +normalized-encodings+
               :do (loop
                     :for key :in keys
                     :when (string-equal key string :start2 start :end2 end)
                       :do (return-from found encoding)))))

         (scan-qp-array (string start end)
           (let ((buffer (get-byte-array)))
             (setf (fill-pointer buffer) 0)
             (loop
               :with p := start :while (< p end)
               :do (let ((char (char string p)))
                     (if (not (eql char #\=))
                         (progn
                           (vector-push-extend (char-code char) buffer)
                           (incf p))
                         (let ((d1 (digit-char-p (char string (+ p 1)) 16))
                               (d2 (digit-char-p (char string (+ p 2)) 16)))
                           (vector-push-extend (logior (ash d1 4) d2) buffer)
                           (incf p 3)))))
             buffer))
         
         (replacer (string target-start target-end match-start match-end reg-starts reg-ends)
           (declare (ignore target-start target-end))
           (let ((encoding (normalize-encoding string (aref reg-starts 0) (aref reg-ends 0)))
                 (using-Q (not (null (aref reg-starts 1)))))
             (cond
               ((not encoding) (subseq string match-start match-end))
               ((not using-Q)
                (let ((byte-array (base64-string-to-usb8-array (subseq string (aref reg-starts 2) (aref reg-ends 2)))))
                  (octets-to-string byte-array :encoding encoding)))
               (t
                (let ((byte-array (scan-qp-array string (aref reg-starts 1) (aref reg-ends 1))))
                  (octets-to-string byte-array :encoding encoding))))))

         (process-string ()
           (cl-ppcre:regex-replace-all #.(concatenate 'string 
                                                 "=[?]([a-zA-Z0-9.@[\\]!#$%&'*+-/=^_`{|}~-]+)"
                                                 "[?](?:(?:[qQ][?]((?:[a-zA-Z0-9.@[\\]!#$%&'*+-/^_`{|}~-]|=[0-9a-fA-F][0-9a-fA-F])+))"
                                                 "|(?:[bB][?]([a-zA-Z0-9+/=]+)))"
                                                 "[?]=")
                                       string #'replacer
                                       :start start :end end :preserve-case nil
                                       :simple-calls nil :element-type 'character)))
      (loop
        :with after-= := nil
        :for index :upfrom start :below end
        :for char := (char string index)
        :do (cond
              ((eql char #\?) 
               (if after-= (return-from decode-=???=-words (process-string))
                   (setf after-= nil)))
              ((eql char #\=) (setf after-= t))
              (t (setf after-= nil)))
        :finally (return (if (and (zerop start) (= end (length string)))
                             string (subseq string start end)))))))

     
                    
               
