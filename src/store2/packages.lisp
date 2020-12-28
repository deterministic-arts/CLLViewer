
(defpackage #:cll-model
  (:use)
  (:export #:mailbox #:mailboxp #:mailbox-equal #:mailbox-hash #:mailbox-string
           #:mailbox-host #:mailbox-name #:parse-mailbox #:make-mailbox #:host
           #:hostp #:host-string #:parse-host #:host-equal #:host-hash))

(defpackage #:cll-model-internals
  (:shadow #:invoke-with-transaction #:with-transaction #:transaction)
  (:local-nicknames (#:sql #:sqlite) (#:db #:darts.lib.sqlite-connection)
                    (#:email #:darts.lib.email-address) (#:neta #:darts.lib.network-address))
  (:use #:common-lisp #:cll-model #:cll-scanner #:cll-utils #:darts.lib.tools
        #:bordeaux-threads #:darts.lib.sqlite-connection #:sqlite #:split-sequence)
  (:export))
