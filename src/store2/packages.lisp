
(defpackage #:cll-model
  (:use)
  (:import-from #:darts.lib.sqlite-connection #:with-transaction #:invoke-with-transaction)
  (:export #:mailbox #:mailboxp #:mailbox-equal #:mailbox-hash #:mailbox-string
           #:mailbox-host #:mailbox-name #:parse-mailbox #:make-mailbox #:host
           #:hostp #:host-string #:parse-host #:host-equal #:host-hash #:store
           #:storep #:open-store #:close-store #:with-store #:invoke-with-store
           #:transaction #:invoke-with-transaction #:with-transaction))

(defpackage #:cll-model-internals
  (:shadowing-import-from #:cll-model #:transaction #:with-transaction #:invoke-with-transaction)
  (:local-nicknames (#:sql #:sqlite)
                    (#:db #:darts.lib.sqlite-connection)
                    (#:email #:darts.lib.email-address)
                    (#:neta #:darts.lib.network-address))
  (:use #:common-lisp #:cll-model #:cll-scanner #:cll-utils #:darts.lib.tools
        #:bordeaux-threads #:darts.lib.sqlite-connection #:sqlite #:split-sequence)
  (:export))
