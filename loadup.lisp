
(cl-user::qload '(#:cl-ppcre #:split-sequence #:local-time #:darts.lib.tools
                  #:optima.ppcre #:cl-base64 #:babel #:cl-date-time-parser #:zlib
                  #:flexi-streams #:darts.lib.email-address #:zcdb #:trivial-open-browser
                  #:puri #:darts.lib.mimetypes #:darts.lib.sqlite-utilities
                  #:clim-font-awesome #:clim-debugger #:darts.lib.calendar
                  #:darts.lib.calendar-local-time))

(clim-symbol-font:register-symbol-font)

(progn
  (load "src/utilities/package")
  (load "src/utilities/utilities")
  (load "src/utilities/progress"))

(progn
  (load "src/clim-utilities/packages")
  (load "src/clim-utilities/datetime")
  (load "src/clim-utilities/hierarchy"))

(progn
  (load "src/indexer/package")
  (load "src/indexer/msgid")
  (load "src/indexer/scanfile")
  (load "src/indexer/makedb")
  (load "src/indexer/checks"))

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

