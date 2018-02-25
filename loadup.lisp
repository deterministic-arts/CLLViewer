
(loop
  :for spec :in '(#:cl-ppcre #:split-sequence #:local-time #:darts.lib.trivia
                  #:darts.lib.annotatable #:optima.ppcre #:cl-base64
                  #:babel #:cl-date-time-parser #:zlib #:flexi-streams
                  #:darts.lib.email-address #:zcdb #:trivial-open-browser #:puri
                  (#:darts.lib.mimetypes #:darts.lib.mime-type)
                  (#:darts.lib.sqlite-utilities #:darts.lib.sqlite-connection)
                  #:clim-font-awesome)
  :do (multiple-value-bind (system package) (if (atom spec) (values spec spec) (values (first spec) (second spec)))
        (unless (find-package package)
          (ql:quickload system))))

(clim-symbol-font:register-symbol-font)

(progn
  (load "src/utilities/package")
  (load "src/utilities/datetime2")
  (load "src/utilities/utilities")
  (load "src/utilities/progress"))

(progn
  (load "src/clim-utilities/packages")
  (load "src/clim-utilities/datetime"))

(progn
  (load "src/indexer/package")
  (load "src/indexer/msgid")
  (load "src/indexer/scanfile"))

(progn
  (load "src/store/package")
  (load "src/store/store"))

(progn
  (load "src/viewer/package")
  (load "src/viewer/definitions")
  (load "src/viewer/progress")
  (load "src/viewer/ptypes")
  (load "src/viewer/message")
  (load "src/viewer/listener"))

