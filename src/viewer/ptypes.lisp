
(in-package "CLL-VIEWER")

(define-presentation-type msgid ()
  :description "Message ID")

(define-presentation-method present ((object msgid) (type msgid) stream view &key acceptably)
  (declare (ignore acceptably))
  (format stream "<~A>" (msgid-string object)))



(define-presentation-type uri ()
  :description "URI")

(define-presentation-method present ((object uri) (type uri) stream view &key acceptably)
  (if acceptably
      (write-token (render-uri object nil) stream :acceptably acceptably)
      (render-uri object stream)))

(define-presentation-method accept ((type uri) stream view &key (default nil have-default) (default-type type))
  (let ((buffer (make-array 80 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      :for char := (peek-char nil stream nil nil) 
      :while (and char
                  (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
                      (position char "!$@%&/()?#~*+;.:-_=")))
      :do (vector-push-extend (read-char stream) buffer))
    (cond
      ((zerop (length buffer))
       (if have-default
           (values default default-type)
           (simple-parse-error "expected an instance of ~S" type)))
      (t (handler-case (values (parse-uri buffer) type)
           (error () (simple-parse-error "~S is not a well-formed instance of ~S" 
                                         buffer type)))))))

(define-command (com-browse-url 
                  :name "Browse URL"
                  :command-table global-commands) ((url 'uri :gesture (:select
                                                                       :tester ((object) (member (uri-scheme object) '(:http :https :ftp)))
                                                                       :documentation "Open")))
  (open-browser (render-uri url nil)))
