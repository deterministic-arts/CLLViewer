
(defpackage #:cll-scanner
  (:use #:common-lisp #:cl-ppcre #:split-sequence
        #:darts.lib.email-address #:darts.lib.media-types #:cl-date-time-parser 
        #:flexi-streams #:cll-utils)
  (:export #:split-message #:load-message-blob #:map-over-file-message-blobs
           #:do-file-message-blobs #:read-file-message-blob))


(defpackage #:cll-indexer
  (:shadowing-import-from #:darts.lib.sqlite-connection #:with-transaction)
  (:local-nicknames (#:neta #:darts.lib.network-address))
  (:use #:common-lisp #:cl-ppcre #:split-sequence #:optima #:optima.ppcre
        #:sqlite #:darts.lib.sqlite-connection #:darts.lib.email-address 
        #:darts.lib.tools #:darts.lib.media-types #:cl-date-time-parser 
        #:zlib #:flexi-streams #:cll-utils
        #:cll-scanner)
  (:export 
    #:msgid #:msgidp 
    #:make-msgid #:msgid-hash #:msgid= #:msgid< #:msgid<= #:msgid>= 
    #:msgid> #:msgid/= #:msgid-string #:parse-msgid #:parse-msgid-list
    #:parse-date-time #:update-message-counters))
