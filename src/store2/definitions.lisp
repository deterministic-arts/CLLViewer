
(in-package #:cll-model-internals)

(defclass store (property-support) ())
(defclass transaction (property-support) ())


(deftype host ()
  '(or neta:host-name neta:ipv4-address neta:ipv6-address))

(defun hostp (object)
  (typep object 'host))

(defun parse-host (string &key (start 0) end)
  (let* ((string (string string))
         (end (or end (length string)))
         (length (- end start)))
    (or (neta:parse-host-name string :junk-allowed t)
        (and (plusp length) (eql #\[ (char string 0)) (eql #\] (char string (1- length)))
             (or (neta:parse-ipv4-address string :start 1 :end (1- length) :junk-allowed t)
                 (neta:parse-ipv6-address string :start 1 :end (1- length) :junk-allowed t))))))

(defgeneric host-string (object)
  (:method ((object neta:host-name)) (neta:address-string object))
  (:method ((object neta:ipv4-address)) (neta:address-string object :prefix #\[ :suffix #\]))
  (:method ((object neta:ipv6-address)) (neta:address-string object :prefix #\[ :suffix #\])))

(defgeneric host (object)
  (:method ((object neta:host-name)) object)
  (:method ((object neta:ipv4-address)) object)
  (:method ((object neta:ipv6-address)) object)
  (:method ((object string)) (or (parse-host object) (call-next-method)))
  (:method ((object symbol)) (or (parse-host (string object)) (call-next-method)))
  (:method ((object character)) (or (parse-host (string object)) (call-next-method)))
  (:method ((object t))
    (error 'simple-type-error
           :datum object :expected-type 'host
           :format-control "~S is not a supported email address host"
           :format-arguments (list object))))

(declaim (inline host-equal host-hash))

(defun host-equal (h1 h2)
  (neta:address-equal h1 h2))

(defun host-hash (object)
  (neta:address-hash object))

(sb-ext:define-hash-table-test host-equal host-hash)

(defstruct (mailbox (:conc-name nil) (:copier nil) (:predicate mailboxp)
                    (:constructor make-mailbox-1 (mailbox-string mailbox-name mailbox-host
                                                  mailbox-hash)))
  (mailbox-plist nil :type list)
  (mailbox-string "" :type simple-string :read-only t)
  (mailbox-name "" :type simple-string :read-only t)
  (mailbox-host (error "missing") :type host :read-only t)
  (mailbox-hash 0 :type fixnum :read-only t))

(define-structure-property-list mailbox mailbox-plist)

(defvar *mailbox-lock* (make-lock))
(defvar *mailbox-table* (make-hash-table :test 'equal :weakness :value))

(defun parse-mailbox (string &key (start 0) end)
  (let ((end (or end (length string))))
    (multiple-value-bind (name host display)
        (email:parse-rfc5322-mailbox string :start start :end end :allow-unicode nil
                                            :allow-obsolete-syntax t)
      (if (not (and host name))
          (values nil nil)
          (let ((host (parse-host host)))
            (if (not host)
                (values nil nil)
                (let* ((string (format nil "~A@~A"
                                       (email:escape-local-part name)
                                       (host-string host)))
                       (hash (sxhash string))
                       (object (with-lock-held (*mailbox-lock*)
                                 (or (gethash string *mailbox-table*)
                                     (setf (gethash string *mailbox-table*)
                                           (make-mailbox-1 string name host hash))))))
                  (values object display))))))))

(defun make-mailbox (name host)
  (let* ((host (host host))
         (name (string name))
         (string (format nil "~A@~A" (email:escape-local-part name) (host-string host)))
         (hash (sxhash string)))
    (with-lock-held (*mailbox-lock*)
      (or (gethash string *mailbox-table*)
          (setf (gethash string *mailbox-table*)
                (make-mailbox-1 string name host hash))))))

(defun mailbox-equal (mb1 mb2)
  (eq mb1 mb2))

(sb-ext:define-hash-table-test mailbox-equal mailbox-hash)

(defmethod print-object ((object mailbox) stream)
  (if (not *print-escape*)
      (write-string (mailbox-string object) stream)
      (print-unreadable-object (object stream :type t :identity nil)
        (prin1 (mailbox-string object) stream))))

(defgeneric mailbox (object)
  (:method ((object mailbox)) object)
  (:method ((object string)) (or (parse-mailbox object) (call-next-method)))
  (:method ((object t))
    (error 'simple-type-error
           :datum object :expected-type 'mailbox
           :format-control "~S is not a supported mailbox designator"
           :format-arguments (list object))))
