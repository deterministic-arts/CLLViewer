
(defpackage "LOCAL-DATE-TIME"
  (:use "COMMON-LISP")
  (:export "LOCAL-DATE" "LOCAL-TIME" "LOCAL-DATE-TIME" "LOCAL-DATE=" "LOCAL-DATE/="
           "LOCAL-DATE<" "LOCAL-DATE<=" "LOCAL-DATE>=" "LOCAL-DATE>" "LOCAL-DATE-YEAR"
           "LOCAL-DATE-MONTH" "LOCAL-DATE-DAY" "LOCAL-DATE-WEEKDAY" "LOCAL-TIME="
           "LOCAL-TIME/=" "LOCAL-TIME<" "LOCAL-TIME<=" "LOCAL-TIME>=" "LOCAL-TIME>"
           "LOCAL-DATE-TIME=" "LOCAL-DATE-TIME<" "LOCAL-DATE-TIME>" "LOCAL-DATE-TIME<="
           "LOCAL-DATE-TIME>=" "LOCAL-DATE-TIME/=" "LOCAL-TIME-HASH" "LOCAL-DATE-HASH"
           "LOCAL-DATE-TIME-HASH" "LOCAL-YEAR" "LOCAL-MONTH" "LOCAL-DAY" "LOCAL-WEEKDAY"
           "LOCAL-HOUR" "LOCAL-MINUTE" "LOCAL-SECOND" "LOCAL-NANOS" "LOCAL-MILLISECOND"
           "LOCAL-MICROSECOND" "LOCAL-NANOSECOND" "TEMPORAL=" "TEMPORAL<=" "TEMPORAL<"
           "TEMPORAL>" "TEMPORAL>=" "TEMPORAL/=" "POSIX-TIME-TO-LOCAL-DATE-TIME"
           "POSIX-TIME-TO-LOCAL-DATE" "POSIX-TIME-TO-LOCAL-TIME" "TEMPORAL-TO-POSIX-TIME"
           "UNIVERSAL-TIME-TO-LOCAL-DATE-TIME" "UNIVERSAL-TIME-TO-LOCAL-DATE"
           "UNIVERSAL-TIME-TO-LOCAL-TIME" "TEMPORAL-TO-UNIVERSAL-TIME" "PRINT-LOCAL-DATE"
           "PRINT-LOCAL-TIME" "PRINT-LOCAL-DATE-TIME" "ADD-SECONDS" "ADD-TIME-UNIT"
           "BEGINNING-OF-MONTH" "END-OF-MONTH" "MAKE-LOCAL-DATE" "MAKE-LOCAL-TIME"
           "MAKE-LOCAL-DATE-TIME"))

(in-package "LOCAL-DATE-TIME")

(defgeneric local-year (object))
(defgeneric local-month (object))
(defgeneric local-day (object))
(defgeneric local-weekday (object))
(defgeneric local-hour (object))
(defgeneric local-minute (object))
(defgeneric local-second (object))
(defgeneric local-nanos (object))

(defun local-millisecond (object)
  (nth-value 0 (floor (local-nanos object) 1000000)))

(defun local-microsecond (object)
  (mod (floor (local-nanos object) 1000) 1000))

(defun local-nanosecond (object)
  (mod (local-nanos object) 1000))

(defgeneric local-date (object &key))
(defgeneric local-time (object &key))
(defgeneric local-date-time (object &key))

(defgeneric temporal< (ob1 ob2))
(defgeneric temporal= (ob1 ob2))

(declaim (inline temporal> temporal<= temporal>= temporal/=))

(defun temporal> (ob1 ob2) (temporal< ob2 ob1))
(defun temporal<= (ob1 ob2) (not (temporal< ob2 ob1)))
(defun temporal>= (ob1 ob2) (not (temporal< ob1 ob2)))
(defun temporal/= (ob1 ob2) (not (temporal= ob1 ob2)))


;;;
;;; references:
;;; - http://howardhinnant.github.io/date_algorithms.html
;;;
;;; an instant is a singular point on the abstract time line. instants are
;;; measured as nanoseconds since the "epoch", which is midnight, march 1st, 2000.
;;; internally, we treat the year to range from  march 1st (first day of the
;;; year) to february, 28th (or 29th, last day of the year)
;;;
;;; i have changed the terms somewhat compared to the original description. what's
;;; called an "era" there, i will call a "cycle" in the code below. that's
;;; simply because the term "era" is usually associated with a different thing
;;; (bce, ce).

(declaim (ftype (function ((integer 1 12) (integer 1 31)) (integer 0 366)) day-of-mar1st-year)
         (inline day-of-mar1st-year))

(defconstant +seconds-per-minute+ 60)
(defconstant +seconds-per-hour+ (* 60 60))
(defconstant +seconds-per-day+ (* 24 60 60))
(defconstant +seconds-per-week+ (* 7 24 60 60))
(defconstant +nanos-per-second+ 1000000000)
(defconstant +years-per-cycle+ 400)
(defconstant +days-per-cycle+ 146097)
(defconstant +min-local-year+ -999999999)
(defconstant +max-local-year+ 999999999)
(defconstant +min-instant-second+ -31557015119088000)
(defconstant +max-instant-second+ 31556888912534399)
(defconstant +universal-time-offset+ 3160857600)
(defconstant +posix-time-offset+ 951868800)

(deftype epoch-second ()
  `(integer ,+min-instant-second+ ,+max-instant-second+))

(declaim (ftype (function (t) epoch-second) epoch-second))

(defun epoch-second (value)
  (if (typep value 'epoch-second)
      value
      (error 'simple-type-error
             :datum value :expected-type 'epoch-second
             :format-control "~s is not a valid seconds-since-epoch value"
             :format-arguments (list value))))

(defun day-of-mar1st-year (month day)
  (+ (truncate (+ (* 153 (+ month (if (> month 2) -3 9))) 2) 5) day -1))

(defmacro declaring-let* ((&rest bindings) &body body)
  (if (null bindings)
      `(let () ,@body)
      (destructuring-bind ((var init &optional (type 't have-type)) &rest more) bindings
        (if (not have-type)
            `(let ((,var ,init))
               (declaring-let* ,more
                 ,@body))
            (let ((temp (gensym (symbol-name var))))
              `(let ((,var (let ((,temp ,init))
                             (if (typep ,temp ',type) ,temp
                                 (error 'simple-type-error
                                        :datum ,temp :expected-type ',type
                                        :format-control "value ~s for variable ~s is not of expected type ~s"
                                        :format-arguments (list ,temp ',var ',type))))))
                 (declare (type ,type ,var))
                 (declaring-let* ,more
                                 ,@body)))))))

(defun epoch-seconds-weekday (value)
  (declare (optimize (speed 3) (debug 0)))
  (let ((value (epoch-second value)))
    (let* ((days-since-epoch (floor value +seconds-per-day+))
           (day-of-cycle (nth-value 1 (floor days-since-epoch +days-per-cycle+))))
      (nth-value 1 (floor (+ 3 day-of-cycle) 7)))))

(defun year/month/day-weekday (year month day)
  (let ((year (- year (if (> month 2) 2000 2001))))
    (multiple-value-bind (cycle year-of-cycle) (floor year +years-per-cycle+)
      (declare (ignore cycle))
      (let* ((day-of-year (day-of-mar1st-year month day))
             (day-of-cycle (- (+ (* 365 year-of-cycle) (truncate year-of-cycle 4) day-of-year) (truncate year-of-cycle 100))))
        (nth-value 1 (floor (+ 3 day-of-cycle) 7))))))

(defmacro encoding-epoch-seconds (year-form month-form day-form &optional (hour-form 0) (minute-form 0) (second-form 0))
  (let ((year (gensym "year")) (month (gensym "month")) (day (gensym "day"))
        (hour (gensym "hour")) (minute (gensym "minute")) (second (gensym "second"))
        (cycle (gensym "cycle")) (year-of-cycle (gensym "year-of-cycle"))
        (day-of-year (gensym "day-of-year")) (day-of-cycle (gensym "day-of-cycle"))
        (days (gensym "days")))
    `(let ((,year ,year-form) (,month ,month-form) (,day ,day-form)
           (,hour ,hour-form) (,minute ,minute-form) (,second ,second-form))
       (multiple-value-bind (,cycle ,year-of-cycle) (floor (- ,year (if (> ,month 2) 2000 2001)) +years-per-cycle+)
         (let* ((,day-of-year (day-of-mar1st-year ,month ,day))
                (,day-of-cycle (- (+ (* 365 ,year-of-cycle) (truncate ,year-of-cycle 4) ,day-of-year) (truncate ,year-of-cycle 100)))
                (,days (+ (* ,cycle +days-per-cycle+) ,day-of-cycle)))
           (declare (type (integer 0 366) ,day-of-year))
           (+ (* +seconds-per-day+ ,days)
              (+ ,second (* ,minute +seconds-per-minute+) (* ,hour +seconds-per-hour+))))))))
  
(defun encode-epoch-seconds (year month day &optional (hour 0) (minute 0) (second 0))
  (declare (optimize (speed 3) (debug 0)))
  (declaring-let* ((year year (integer -1000000000 1000000000))
                   (month month (integer 1 12))
                   (day day (integer 1 31))
                   (hour hour (integer 0 23))
                   (minute minute (integer 0 59))
                   (second second (integer 0 59)))
    (encoding-epoch-seconds year month day hour minute second)))

(defun decode-epoch-seconds (value)
  (declare (optimize (speed 3) (debug 0)))
  (if (not (typep value 'epoch-second))
      (error 'simple-type-error
             :datum value :expected-type 'epoch-second
             :format-control "~s is not a valid seconds-since-epoch value"
             :format-arguments (list value))
      (locally (declare (type epoch-second value))
        (multiple-value-bind (day second-of-day) (floor value +seconds-per-day+)
          (multiple-value-bind (cycle day-of-cycle) (floor day +days-per-cycle+)
            (let* ((year-of-cycle (truncate (+ day-of-cycle
                                               (- (truncate day-of-cycle 1460))
                                               (truncate day-of-cycle 36524)
                                               (- (truncate day-of-cycle (1- +days-per-cycle+))))
                                            365))
                   (year (+ year-of-cycle (* cycle +years-per-cycle+))) ; not yet, but see below
                   (day-of-year (+ (- day-of-cycle
                                      (* 365 year-of-cycle)
                                      (truncate year-of-cycle 4))
                                   (truncate year-of-cycle 100)))
                   (mp (truncate (+ (* 5 day-of-year) 2) 153))
                   (day-of-month (1+ (- day-of-year (truncate (+ (* 153 mp) 2) 5))))
                   (month (+ mp (if (< mp 10) 3 -9))))
              (multiple-value-bind (hour rest) (floor second-of-day +seconds-per-hour+)
                (multiple-value-bind (minute second) (floor rest +seconds-per-minute+)
                  (values (+ year (if (> month 2) 2000 2001)) month day-of-month
                          hour minute second
                          (nth-value 1 (floor (+ 3 day-of-cycle) 7)))))))))))

(defun remove-keywords (plist keys)
  (loop
     for (key value) on plist by #'cddr
     unless (member key keys)
       nconcing (list key value)))


(deftype local-year () `(integer ,+min-local-year+ ,+max-local-year+))
(deftype local-month () `(integer 1 12))
(deftype local-hour () `(integer 0 23))
(deftype local-minute () `(integer 0 59))
(deftype local-second () `(integer 0 59))
(deftype local-nanos () `(integer 0 999999999))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun days-in-month (year month)
    (ecase month
      ((1 3 5 7 8 10 12) 31)
      ((4 6 9 11) 30)
      ((2) (if (and (zerop (mod year 4)) (or (not (zerop (mod year 100))) (zerop (mod year 400))))
               29 28)))))

(deftype local-month-day (year month)
  `(integer 1 ,(days-in-month year month)))



(defstruct (local-date (:copier nil) (:conc-name nil)
                       (:constructor make-local-date-1 (ldate-value &optional ldate-weekday)))
  (ldate-value (error "missing date value") :type (signed-byte 32) :read-only t)
  (ldate-weekday nil :type (or null (integer 0 6))))

(defstruct (local-time (:copier nil) (:conc-name nil)
                       (:constructor make-local-time-1 (ltime-value ltime-nanos)))
  (ltime-value 0 :type (unsigned-byte 32) :read-only t)
  (ltime-nanos 0 :type (integer 0 999999999) :read-only t))

(defstruct (local-date-time (:copier nil) (:conc-name nil)
                            (:constructor make-local-date-time-1 (ldt-date ldt-time)))
  (ldt-date (error "missing date") :type local-date :read-only t)
  (ldt-time (error "missing time") :type local-time :read-only t))

(declaim (inline local-date-year local-date-month local-date-day
                 local-time-hour local-time-minute local-time-second
                 local-time-nanos))

(defun local-date-year (object)
  (ash (ldate-value object) -16))

(defun local-date-month (object)
  (ldb (byte 8 8) (ldate-value object)))

(defun local-date-day (object)
  (ldb (byte 8 0) (ldate-value object)))

(defun local-date-weekday (object)
  (or (ldate-weekday object)
      (let* ((value (ldate-value object))
             (year (ash value -16))
             (month (ldb (byte 8 8) value))
             (day (ldb (byte 8 0) value)))
        (setf (ldate-weekday object) (mod (+ (year/month/day-weekday year month day) 6) 7)))))

(defun local-time-hour (object)
  (ldb (byte 8 16) (ltime-value object)))

(defun local-time-minute (object)
  (ldb (byte 8 8) (ltime-value object)))

(defun local-time-second (object)
  (ldb (byte 8 0) (ltime-value object)))

(defun local-time-nanos (object)
  (ltime-nanos object))

(defmethod local-year ((object local-date)) (local-date-year object))
(defmethod local-month ((object local-date)) (local-date-month object))
(defmethod local-day ((object local-date)) (local-date-day object))
(defmethod local-weekday ((object local-date)) (local-date-weekday object))

(defmethod local-year ((object local-date-time)) (local-date-year (ldt-date object)))
(defmethod local-month ((object local-date-time)) (local-date-month (ldt-date object)))
(defmethod local-day ((object local-date-time)) (local-date-day (ldt-date object)))
(defmethod local-weekday ((object local-date-time)) (local-date-weekday (ldt-date object)))

(defmethod local-hour ((object local-time)) (local-time-hour object))
(defmethod local-minute ((object local-time)) (local-time-minute object))
(defmethod local-second ((object local-time)) (local-time-second object))
(defmethod local-nanos ((object local-time)) (local-time-nanos object))

(defmethod local-hour ((object local-date-time)) (local-time-hour (ldt-time object)))
(defmethod local-minute ((object local-date-time)) (local-time-minute (ldt-time object)))
(defmethod local-second ((object local-date-time)) (local-time-second (ldt-time object)))
(defmethod local-nanos ((object local-date-time)) (local-time-nanos (ldt-time object)))


(macrolet ((define-comparison (conc-name (ob1 ob2) =-form <-form)
             (flet ((sconc (suff) (intern (format nil "~a~a" (symbol-name conc-name) (symbol-name suff)))))
               (let ((func= (sconc '=)) (func/= (sconc '/=))
                     (func< (sconc '<)) (func<= (sconc '<=))
                     (func> (sconc '>)) (func>= (sconc '>=)))
                 `(progn
                    (defun ,func= (,ob1 ,ob2) ,=-form)
                    (defun ,func< (,ob1 ,ob2) ,<-form)
                    (declaim (inline ,func/= ,func> ,func>= ,func<=))
                    (defun ,func> (x y) (,func< y x))
                    (defun ,func>= (x y) (not (,func< x y)))
                    (defun ,func<= (x y) (not (,func< y x)))
                    (defun ,func/= (x y) (not (,func= x y))))))))
  
  (define-comparison local-date (ob1 ob2)
    (eql (ldate-value ob1) (ldate-value ob2))
    (< (ldate-value ob1) (ldate-value ob2)))

  (define-comparison local-time (ob1 ob2)
    (and (eql (ltime-value ob1) (ltime-value ob2))
         (eql (ltime-nanos ob1) (ltime-nanos ob2)))
    (let ((v1 (ltime-value ob1)) (v2 (ltime-value ob2)))
      (cond
        ((< v1 v2) t)
        ((> v1 v2) nil)
        (t (< (ltime-nanos ob1) (ltime-nanos ob2))))))

  (define-comparison local-date-time (ob1 ob2)
    (and (local-date= (ldt-date ob1) (ldt-date ob2))
         (local-time= (ldt-time ob1) (ldt-time ob2)))
    (let ((dv1 (ldate-value (ldt-date ob1))) (dv2 (ldate-value (ldt-date ob2))))
      (cond
        ((< dv1 dv2) t)
        ((> dv1 dv2) nil)
        (t (local-time< (ldt-time ob1) (ldt-time ob2)))))))

(defun local-date-hash (object)
  (logand #x7fffffff (ldate-value object)))

(defun local-time-hash (object)
  (sxhash (logxor (ltime-value object) (ltime-nanos object))))

(defun local-date-time-hash (object)
  (sxhash (logxor (local-date-hash (ldt-date object)) (local-time-hash (ldt-time object)))))



(defparameter +midnight+ (make-local-time-1 0 0))
(defparameter +almost-midnight+ (make-local-time-1 (logior (ash 23 16) (ash 59 8) 59) 999999999))
(defparameter +epoch-date+ (make-local-date-1 (logior (ash 2000 16) (ash 3 8) 1) 2))
(defparameter +epoch-date-time+ (make-local-date-time-1 +epoch-date+ +midnight+))

(macrolet ((with-asserted-type ((value type) &body body)
             `(if (not (typep ,value ,type))
                  (error 'type-error :datum ,value :expected-type ,type)
                  (progn ,@body))))

  (defun make-local-date (year month day)
    (with-asserted-type (year 'local-year)
      (with-asserted-type (month 'local-month)
        (with-asserted-type (day `(local-month-day ,year ,month))
          (make-local-date-1 (logior (ash year 16) (ash month 8) day))))))

  (defun make-local-time (hour minute second &optional (nanos 0))
    (with-asserted-type (hour 'local-hour)
      (with-asserted-type (minute 'local-minute)
        (with-asserted-type (second 'local-second)
          (with-asserted-type (nanos 'local-nanos)
            (if (= 0 hour minute second nanos)
                +midnight+
                (make-local-time-1 (logior (ash hour 16) (ash minute 8) second)
                                   nanos))))))))
  
(defun make-local-date-time (year month day &optional (hour 0) (minute 0) (second 0) (nanos 0))
  (make-local-date-time-1 (make-local-date year month day)
                          (make-local-time hour minute second nanos)))
  


(defmethod local-time (object &key)
  (error 'type-error :datum object :expected-type 'local-time))

(defmethod local-date (object &key)
  (error 'type-error :datum object :expected-type 'local-date))

(defmethod local-date-time (object &key)
  (error 'type-error :datum object :expected-type 'local-date-time))

(defmethod local-time ((object local-date-time) &key) (ldt-time object))
(defmethod local-time ((object local-time) &key) object)

(defmethod local-date ((object local-date-time) &key) (ldt-date object))
(defmethod local-date ((object local-date) &key) object)

(defmethod local-date-time ((object local-date-time) &key) object)

(defmethod local-date-time ((object local-date) &rest options &key (defaults +epoch-date-time+))
  (make-local-date-time-1 object
                          (apply #'local-time defaults (remove-keywords options ':defaults))))

(defmethod local-date-time ((object local-time) &rest options &key (defaults +epoch-date-time+))
  (make-local-date-time-1 (apply #'local-date defaults (remove-keywords options ':defaults))
                          object))



(defgeneric encode-temporal (object)
  (:method ((object local-time))
    (let* ((value (ltime-value object))
           (h (ldb (byte 8 16) value))
           (m (ldb (byte 8 8) value))
           (s (ldb (byte 8 0) value)))
      (values (+ (* 60 60 h) (* 60 m) s) (ltime-nanos object))))
  (:method ((object local-date))
    (let* ((value (ldate-value object))
           (y (ash value -16))
           (m (ldb (byte 8 8) value))
           (d (ldb (byte 8 0) value)))
      (values (encode-epoch-seconds y m d) 0)))
  (:method ((object local-date-time))
    (let ((date (ldt-date object))
          (time (ldt-time object)))
      (values (encode-epoch-seconds (local-date-year date) (local-date-month date) (local-date-day date)
                                    (local-time-hour time) (local-time-minute time) (local-time-second time))
              (local-time-nanos time)))))

(defun temporal-to-posix-time (value)
  (+ (encode-temporal value) +posix-time-offset+))

(defun temporal-to-universal-time (value)
  (+ (encode-temporal value) +universal-time-offset+))

(defun posix-time-to-local-date-time (value &optional (nanos 0))
  (multiple-value-bind (y m d hh mm ss) (decode-epoch-seconds (- value +posix-time-offset+))
    (make-local-date-time y m d hh mm ss nanos)))

(defun posix-time-to-local-date (value)
  (multiple-value-bind (y m d) (decode-epoch-seconds (- value +posix-time-offset+))
    (make-local-date y m d)))

(defun posix-time-to-local-time (value &optional (nanos 0))
  (multiple-value-bind (y m d hh mm ss) (decode-epoch-seconds (- value +posix-time-offset+))
    (declare (ignore y m d))
    (make-local-time hh mm ss nanos)))

(defun universal-time-to-local-date-time (value &optional (nanos 0))
  (multiple-value-bind (y m d hh mm ss) (decode-epoch-seconds (- value +universal-time-offset+))
    (make-local-date-time y m d hh mm ss nanos)))

(defun universal-time-to-local-date (value)
  (multiple-value-bind (y m d) (decode-epoch-seconds (- value +universal-time-offset+))
    (make-local-date-time y m d)))

(defun universal-time-to-local-time (value &optional (nanos 0))
  (multiple-value-bind (y m d hh mm ss) (decode-epoch-seconds (- value +universal-time-offset+))
    (declare (ignore y m d))
    (make-local-time hh mm ss nanos)))


;;;
;;; This code was shamelessly stolen from LOCAL-TIME. Maybe, we should
;;; require local-time as dependency for now, and do local-time::%get-current-time?
;;;

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Allegro common lisp requires some toplevel hoops through which to
  ;; jump in order to call unix's gettimeofday properly.
  (ff:def-foreign-type timeval
      (:struct (tv_sec :long)
               (tv_usec :long)))

  (ff:def-foreign-call
      (allegro-ffi-gettimeofday "gettimeofday")
      ((timeval (* timeval))
       ;; and do this to allow a 0 for NULL
       (timezone :foreign-address))
    :returning (:int fixnum)))

(defun %get-current-time ()
  "Cross-implementation abstraction to get the current time measured from the unix epoch (1/1/1970). Should return (values sec nano-sec)."
  #+allegro
  (flet ((allegro-gettimeofday ()
           (let ((tv (ff:allocate-fobject 'timeval :c)))
             (allegro-ffi-gettimeofday tv 0)
             (let ((sec (ff:fslot-value-typed 'timeval :c tv 'tv_sec))
                   (usec (ff:fslot-value-typed 'timeval :c tv 'tv_usec)))
               (ff:free-fobject tv)
               (values sec usec)))))
    (multiple-value-bind (sec usec) (allegro-gettimeofday)
      (values sec (* 1000 usec))))
  #+cmu
  (multiple-value-bind (success? sec usec) (unix:unix-gettimeofday)
    (assert success? () "unix:unix-gettimeofday reported failure?!")
    (values sec (* 1000 usec)))
  #+sbcl
  (progn
    (multiple-value-bind (sec nsec) (sb-ext:get-time-of-day)
      (values sec (* 1000 nsec))))
  #+(and ccl (not windows))
  (ccl:rlet ((tv :timeval))
    (let ((err (ccl:external-call "gettimeofday" :address tv :address (ccl:%null-ptr) :int)))
      (assert (zerop err) nil "gettimeofday failed")
      (values (ccl:pref tv :timeval.tv_sec) (* 1000 (ccl:pref tv :timeval.tv_usec)))))
  #+abcl
  (multiple-value-bind (sec millis)
      (truncate (java:jstatic "currentTimeMillis" "java.lang.System") 1000)
    (values sec (* millis 1000000)))
  #-(or allegro cmu sbcl abcl (and ccl (not windows)))
  (values (- (get-universal-time)
             ;; CL's get-universal-time uses an epoch of 1/1/1900, so adjust the result to the Unix epoch
             #.(encode-universal-time 0 0 0 1 1 1970 0))
          0))



(defun print-local-date (object &optional (stream *standard-output*))
  (format stream "~D-~2,'0D-~2,'0D" (local-year object) (local-month object) (local-day object))
  object)

(defun print-local-time (object &optional (stream *standard-output*))
  (let ((ns (local-nanos object)))
    (multiple-value-bind (ns* prec)
        (if (zerop ns) (values ns 1)
            (if (zerop (mod ns 1000000))
                (values (floor ns 1000000) 3)
                (if (zerop (mod ns 1000))
                    (values (floor ns 1000) 6)
                    (values ns 9))))
      (format stream "~2,'0D:~2,'0D:~2,'0D.~v,'0D"
              (local-hour object)
              (local-minute object)
              (local-second object)
              prec ns*)))
  object)

(defun print-local-date-time (object &optional (stream *standard-output*))
  (print-local-date object stream)
  (write-char #\T stream)
  (print-local-time object stream)
  object)

(defmethod print-object ((object local-date) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (print-local-date object stream)))

(defmethod print-object ((object local-time) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (print-local-time object stream)))

(defmethod print-object ((object local-date-time) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (print-local-date-time object stream)))



(defun to-seconds-and-nanos (value unit)
  (ecase unit
    ((:nanoseconds :nanosecond) (values 0 value))
    ((:microseconds :microsecond) (values 0 (* value 1000)))
    ((:milliseconds :millisecond) (values 0 (* value 1000000)))
    ((:seconds :second) (values value 0))
    ((:minutes :minute) (values (* value 60) 0))
    ((:hours :hour) (values (* value 3600) 0))
    ((:days :day) (values (* value 86400) 0))
    ((:weeks :week) (values (* value #.(* 86400 7)) 0))))

(defgeneric add-seconds (object seconds &optional nanos))

(defmethod add-seconds ((object local-date-time) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos)) object
      (let ((new-seconds (+ (encode-temporal object) seconds))
            (new-nanos (+ (local-nanos object) nanos)))
        (multiple-value-bind (add-seconds add-nanos) (floor new-nanos 1000000000)
          (let ((pes (+ new-seconds add-seconds)))
            (multiple-value-bind (year month day hour minute second) (decode-epoch-seconds pes)
              (make-local-date-time-1 (make-local-date year month day)
                                      (make-local-time hour minute second add-nanos))))))))

(defmethod add-seconds ((object local-time) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos))
      object
      (let ((new-seconds (+ (encode-temporal object) seconds))
            (new-nanos (+ (local-time-nanos object) nanos)))
        (multiple-value-bind (add-seconds add-nanos) (floor new-nanos 1000000000)
          (multiple-value-bind (unused true-seconds) (floor (+ add-seconds new-seconds) 86400)
            (declare (ignore unused))
            (multiple-value-bind (rest seconds) (floor true-seconds 60)
              (multiple-value-bind (hours minuts) (floor rest 60)
                (make-local-time hours minuts seconds add-nanos))))))))

(defmethod add-seconds ((object local-date) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos))
      object
      (let* ((new-seconds (+ (encode-temporal object) seconds (floor nanos 1000000000)))
             (full-days (floor new-seconds 86400))
             (stretched (* full-days 86400)))
        (multiple-value-bind (year month day) (decode-epoch-seconds stretched)
          (make-local-date year month day)))))


(defun add-time-unit (object value unit)
  (multiple-value-bind (seconds nanos) (to-seconds-and-nanos value unit)
    (add-seconds object seconds nanos)))



(defun ldate-first-day-of-month (object)
  (if (eql 1 (local-date-day object))
      object
      (make-local-date (local-date-year object) (local-date-month object) 1)))

(defun ldate-last-day-of-month (object)
  (let ((max (days-in-month (local-date-year object) (local-date-month object))))
    (if (eql max (local-date-day object))
        object
        (make-local-date (local-date-year object) (local-date-month object) max))))

(defun beginning-of-month (object)
  (etypecase object
    (local-date (ldate-first-day-of-month object))
    (local-date-time
     (let ((a (ldt-date object))
           (i (ldt-time object)))
       (if (and (eql (local-date-day a) 1) (zerop (ltime-value i)) (zerop (ltime-nanos i)))
           object
           (make-local-date-time-1 (ldate-first-day-of-month a)
                                   +midnight+))))))

(defun end-of-month (object)
  (etypecase object
    (local-date (ldate-last-day-of-month object))
    (local-date-time
     (let ((a (ldt-date object))
           (i (ldt-time object)))
       (if (and (eql (local-date-day a) 1)
                (eql (ltime-value i) (ltime-value +almost-midnight+))
                (eql (ltime-nanos i) 999999999))
           object
           (make-local-date-time-1 (ldate-last-day-of-month a)
                                   +almost-midnight+))))))
