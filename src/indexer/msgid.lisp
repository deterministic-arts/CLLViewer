
(in-package "CLL-INDEXER")

#|

message-id      =   "Message-ID:" msg-id CRLF
in-reply-to     =   "In-Reply-To:" 1*msg-id CRLF
references      =   "References:" 1*msg-id CRLF
msg-id          =   [CFWS] "<" id-left "@" id-right ">" [CFWS]
id-left         =   dot-atom-text / obs-id-left
id-right        =   dot-atom-text / no-fold-literal / obs-id-right
no-fold-literal =   "[" *dtext "]"
atom            =   [CFWS] 1*atext [CFWS]
dot-atom-text   =   1*atext *("." 1*atext)
dot-atom        =   [CFWS] dot-atom-text [CFWS]
atext           =   ALPHA / DIGIT /    ; Printable US-ASCII
                       "!" / "#" /     ;  characters not including
                       "$" / "%" /     ;  specials.  Used for atoms.
                       "&" / "'" /
                       "*" / "+" /
                       "-" / "/" /
                       "=" / "?" /
                       "^" / "_" /
                       "`" / "{" /
                       "|" / "}" /
                       "~"
ctext           =   %d33-39 /          ; Printable US-ASCII
                      %d42-91 /          ;  characters not including
                      %d93-126 /         ;  "(", ")", or "\"
                      obs-ctext

ccontent        =   ctext / quoted-pair / comment
comment         =   "(" *([FWS] ccontent) [FWS] ")"

|#

(defstruct (msgid (:copier nil) (:predicate msgidp) (:constructor %make-msgid (string)))
  (string (error "missing string") :type simple-string :read-only t)
  (%hash -1 :type fixnum))

(defmethod print-object ((object msgid) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (msgid-string object)))) 

(defun msgid-hash (object)
  (let ((value (msgid-%hash object)))
    (if (not (minusp value)) value
        (let ((hash (sxhash (msgid-string object))))
          (setf (msgid-%hash object) hash)
          hash))))
  
(defun msgid= (ob1 ob2)
  (or (eq ob1 ob2)
      (string= (msgid-string ob1) (msgid-string ob2))))

(defun msgid/= (ob1 ob2)
  (and (not (eq ob1 ob2))
       (string/= (msgid-string ob1) (msgid-string ob2))))

(defun msgid< (ob1 ob2)
  (string< (msgid-string ob1) (msgid-string ob2)))

(defun msgid<= (ob1 ob2)
  (string<= (msgid-string ob1) (msgid-string ob2)))

(defun msgid>= (ob1 ob2)
  (string>= (msgid-string ob1) (msgid-string ob2)))

(defun msgid> (ob1 ob2)
  (string> (msgid-string ob1) (msgid-string ob2)))

(defun make-msgid (string &key table)
  (let* ((raw-key (typecase string
                    (string string)
                    (symbol (symbol-name string))
                    (character (string string))
                    (t (error 'type-error :datum string :expected-type 'string))))
         (present (and table (gethash raw-key table))))
    (or present
        (let* ((key (coerce raw-key 'simple-string))
               (object (%make-msgid key)))
          (when table (setf (gethash key table) object))
          object))))



#|
The initial versions of this code used actually be RFC 5322 conformant 
parsers for the msgid and msgid-lists. The problem is: our input file is
syntactically completely broken wrt. message ID headers. So here, we go
with a simpler variant.
|#

(defun parse-msgid (string &key (start 0) end table)
  (let* ((string (string string))
         (end (or end (length string)))
         (buffer (make-array (- end start) :element-type 'character :fill-pointer 0)))
    (labels
        ((whitespacep (char)
           (and (position char #.(concatenate 'string '(#\newline #\return #\tab #\space #\< #\> #\,)))
                t))
         (memberp (char)
           (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
               (and (position char ".@[]!#$%&'*+-/=?^_`{|}~")
                    t)))
         (skip-comment (position depth)
           (if (>= position end)
               (values end nil)
               (let ((char (char string position)))
                 (cond
                   ((eql char #\() (skip-comment (1+ position) (1+ depth)))
                   ((eql char #\)) (if (eql depth 1) (values (1+ position) t) (skip-comment (1+ position) (1- depth))))
                   (t (skip-comment (1+ position) depth))))))
         (add (char)
           (vector-push-extend char buffer))
         (parse (position)
           (if (>= position end) 
               (values nil end)
               (let ((char (char string position)))
                 (cond
                   ((whitespacep char) (parse (1+ position)))
                   ((memberp char) (add char) (collect (1+ position)))
                   ((eql char #\() (parse (skip-comment (1+ position) 1)))
                   ((eql char #\)) (parse (1+ position)))
                   (t (values nil position))))))
         (collect (position)
           (if (>= position end)
               (values (make-msgid buffer :table table) end)
               (let ((char (char string position)))
                 (cond
                   ((memberp char) (add char) (collect (1+ position)))
                   (t (values (make-msgid buffer :table table) position)))))))
      (parse start))))


(defun parse-msgid-list (string &key (start 0) end table reversed)
  (let* ((string (string string))
         (end (or end (length string)))
         (elements nil)
         (buffer (make-array (- end start) :element-type 'character :fill-pointer 0)))
    (labels
        ((whitespacep (char)
           (and (position char #.(concatenate 'string '(#\newline #\return #\tab #\space #\< #\> #\,)))
                t))
         (memberp (char)
           (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
               (and (position char ".@[]!#$%&'*+-/=?^_`{|}~")
                    t)))
         (skip-comment (position depth)
           (if (>= position end)
               (values end nil)
               (let ((char (char string position)))
                 (cond
                   ((eql char #\() (skip-comment (1+ position) (1+ depth)))
                   ((eql char #\)) (if (eql depth 1) (values (1+ position) t) (skip-comment (1+ position) (1- depth))))
                   (t (skip-comment (1+ position) depth))))))
         (add (char) 
           (vector-push-extend char buffer))
         (finish () 
           (when (plusp (fill-pointer buffer))
             (push (make-msgid buffer :table table) elements) 
             (setf (fill-pointer buffer) 0)
             t))
         (leave (position)
           (finish)
           (values (if reversed elements (nreverse elements))
                   position))
         (parse (position)
           (if (>= position end) 
               (leave end)
               (let ((char (char string position)))
                 (cond
                   ((whitespacep char) (parse (1+ position)))
                   ((memberp char) (add char) (collect (1+ position)))
                   ((eql char #\() (parse (skip-comment (1+ position) 1)))
                   ((eql char #\)) (parse (1+ position)))
                   (t (leave position))))))
         (collect (position)
           (if (>= position end)
               (leave end)
               (let ((char (char string position)))
                 (cond
                   ((memberp char) (add char) (collect (1+ position)))
                   ((whitespacep char) (finish) (parse (1+ position)))
                   ((eql char #\() (finish) (parse position))
                   ((eql char #\)) (finish) (parse (1+ position)))
                   (t (leave position)))))))
      (parse start))))
