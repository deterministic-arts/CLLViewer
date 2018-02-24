
(in-package "CLL-LOCAL-DATE-TIME")

(defgeneric local-year (object))
(defgeneric local-month (object))
(defgeneric local-day (object))
(defgeneric local-weekday (object))
(defgeneric local-hour (object))
(defgeneric local-minute (object))
(defgeneric local-second (object))
(defgeneric local-nanos (object))
(defgeneric local-date (object &key))
(defgeneric local-time (object &key))
(defgeneric local-date-time (object &key))

(defun local-millisecond (object)
  (nth-value 0 (floor (local-nanos object) 1000000)))

(defun local-microsecond (object)
  (mod (floor (local-nanos object) 1000) 1000))

(defun local-nanosecond (object)
  (mod (local-nanos object) 1000))

(defstruct (local-date (:copier nil) (:conc-name nil)
                       (:constructor make-local-date-1 (ldate-value &optional ldate-weekday)))
  (ldate-value (error "missing date value") :type (unsigned-byte 32) :read-only t)
  (ldate-weekday nil :type (or null (integer 0 6))))

(declaim (inline local-date-year local-date-month local-date-day))

(defun local-date-year (object)
  (ldb (byte 16 16) (ldate-value object)))

(defun local-date-month (object)
  (ldb (byte 8 8) (ldate-value object)))

(defun local-date-day (object)
  (ldb (byte 8 0) (ldate-value object)))

(defun days-in-month (year month)
  (ecase month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    ((2) (if (and (zerop (mod year 4)) (or (not (zerop (mod year 100))) (zerop (mod year 400))))
             29 28))))

(defun make-local-date (year month day)
  (macrolet ((with-asserted-type ((value type) &body body)
               `(if (not (typep ,value ',type))
                    (error 'type-error :datum ,value :expected-type ',type)
                    (progn ,@body))))
    (with-asserted-type (year (integer 1900 2038))
      (with-asserted-type (month (integer 1 12))
        (let ((max-day (days-in-month year month)))
          (if (not (typep day `(integer 1 ,max-day)))
              (error 'type-error :datum day :expected-type `(integer 1 ,max-day))
              (make-local-date-1 (logior (ash year 16) (ash month 8) day))))))))

(defun local-date-to-universal-time (object &optional (tz nil have-tz))
  (apply #'encode-universal-time
         0 0 0 (local-date-day object) (local-date-month object) (local-date-year object)
         (and have-tz (list tz))))

(defun universal-time-to-local-date (utime &optional (tz nil have-tz))
  (multiple-value-bind (u1 u2 u3 day month year weekday)
      (apply #'decode-universal-time utime (and have-tz (list tz)))
    (declare (ignore u1 u2 u3))
    (make-local-date-1 (logior (ash year 16) (ash month 8) day) weekday)))

(defun current-local-date (&optional (tz nil have-tz))
  (multiple-value-bind (u1 u2 u3 day month year weekday)
      (apply #'decode-universal-time (get-universal-time) (and have-tz (list tz)))
    (declare (ignore u1 u2 u3))
    (make-local-date-1 (logior (ash year 16) (ash month 8) day) weekday)))

(defun local-date-weekday (object)
  (or (ldate-weekday object)
      (multiple-value-bind (u1 u2 u3 u4 u5 u6 weekday)
          (decode-universal-time (local-date-to-universal-time object 0) 0)
        (declare (ignore u1 u2 u3 u4 u5 u6))
        (setf (ldate-weekday object) weekday))))

(defmethod local-year ((object local-date)) (local-date-year object))
(defmethod local-month ((object local-date)) (local-date-month object))
(defmethod local-day ((object local-date)) (local-date-day object))
(defmethod local-weekday ((object local-date)) (local-date-weekday object))

(declaim (inline local-date= local-date< local-date<= local-date>= local-date= local-date/=))

(defun local-date= (ob1 ob2)
  (= (ldate-value ob1) (ldate-value ob2)))

(defun local-date/= (ob1 ob2)
  (/= (ldate-value ob1) (ldate-value ob2)))

(defun local-date< (ob1 ob2)
  (< (ldate-value ob1) (ldate-value ob2)))

(defun local-date<= (ob1 ob2)
  (<= (ldate-value ob1) (ldate-value ob2)))

(defun local-date>= (ob1 ob2)
  (>= (ldate-value ob1) (ldate-value ob2)))

(defun local-date> (ob1 ob2)
  (> (ldate-value ob1) (ldate-value ob2)))

(defun local-date-hash (object)
  (sxhash (ldate-value object)))



(defstruct (local-time (:copier nil) (:conc-name nil)
                       (:constructor make-local-time-1 (ltime-value ltime-nanos)))
  (ltime-value 0 :type (unsigned-byte 32) :read-only t)
  (ltime-nanos 0 :type (integer 0 999999999) :read-only t))

(declaim (inline local-time-hour local-time-minute local-time-second local-time-nanos
                 local-time<= local-time>= local-time>))

(defun local-time-hour (object)
  (ldb (byte 8 16) (ltime-value object)))

(defun local-time-minute (object)
  (ldb (byte 8 8) (ltime-value object)))

(defun local-time-second (object)
  (ldb (byte 8 0) (ltime-value object)))

(defun local-time-nanos (object)
  (ltime-nanos object))

(defmethod local-hour ((object local-time)) (local-time-hour object))
(defmethod local-minute ((object local-time)) (local-time-minute object))
(defmethod local-second ((object local-time)) (local-time-second object))
(defmethod local-nanos ((object local-time)) (ltime-nanos object))

(defun make-local-time (hour minute second &optional (nanos 0))
  (macrolet ((with-asserted-type ((value type) &body body)
               `(if (not (typep ,value ',type))
                    (error 'type-error :datum ,value :expected-type ',type)
                    (progn ,@body))))
    (with-asserted-type (hour (integer 0 23))
      (with-asserted-type (minute (integer 0 59))
        (with-asserted-type (second (integer 0 59))
          (with-asserted-type (nanos (integer 0 999999999))
            (make-local-time-1 (logior (ash hour 16) (ash minute 8) second)
                               nanos)))))))

(defun current-local-time (&optional (tz nil have-tz))
  (multiple-value-bind (s m h) (apply #'decode-universal-time (get-universal-time) (and have-tz (list tz)))
    (make-local-time-1 (logior (ash h 16) (ash m 8) s) 0)))

(defun local-time= (ob1 ob2)
  (and (eql (ltime-value ob1) (ltime-value ob2))
       (eql (ltime-nanos ob1) (ltime-nanos ob2))))

(defun local-time< (ob1 ob2)
  (let ((v1 (ltime-value ob1)) (v2 (ltime-value ob2)))
    (cond
      ((< v1 v2) t)
      ((> v1 v2) nil)
      (t (< (ltime-nanos ob1) (ltime-nanos ob2))))))
                   
(defun local-time/= (ob1 ob2) (not (local-time= ob1 ob2)))
(defun local-time<= (ob1 ob2) (not (local-time< ob2 ob1)))
(defun local-time>= (ob1 ob2) (not (local-time< ob1 ob2)))
(defun local-time> (ob1 ob2) (local-time< ob2 ob1))

(defun local-time-hash (object)
  (sxhash (logxor (ltime-value object) (ltime-nanos object))))



(defstruct (local-date-time (:copier nil) (:conc-name nil) (:include local-date)
                            (:constructor make-local-date-time-1 (ldate-value ldate-weekday ldt-time)))
  (ldt-time (error "missing time") :type local-time :read-only t))

(defun current-local-date-time (&optional (tz nil have-tz))
  (multiple-value-bind (ss mm hh d m y wd) (apply #'decode-universal-time (get-universal-time) (and have-tz (list tz)))
    (make-local-date-time-1 (logior (ash y 16) (ash m 8) d) wd
                            (make-local-time-1 (logior (ash hh 16) (ash mm 8) ss) 0))))

(defun make-local-date-time (year month day &optional (hour 0) (minute 0) (second 0) (nanos 0))
  (macrolet ((with-asserted-type ((value type) &body body)
               `(if (not (typep ,value ',type))
                    (error 'type-error :datum ,value :expected-type ',type)
                    (progn ,@body))))
    (with-asserted-type (year (integer 1900 2038))
      (with-asserted-type (month (integer 1 12))
        (let ((max-day (days-in-month year month)))
          (if (not (typep day `(integer 1 ,max-day)))
              (error 'type-error :datum day :expected-type `(integer 1 ,max-day))
              (make-local-date-time-1 (logior (ash year 16) (ash month 8) day) nil
                                      (make-local-time hour minute second nanos))))))))

(defmethod local-hour ((object local-date-time))
  (local-time-hour (ldt-time object)))

(defmethod local-minute ((object local-date-time))
  (local-time-minute (ldt-time object)))

(defmethod local-second ((object local-date-time))
  (local-time-second (ldt-time object)))

(defmethod local-nanos ((object local-date-time))
  (ltime-nanos (ldt-time object)))

(defun local-date-time-to-universal-time (object &optional (tz nil have-tz))
  (apply #'encode-universal-time
         (local-second object) (local-minute object) (local-hour object)
         (local-day object) (local-month object) (local-year object)
         (and have-tz (list tz))))

(defun universal-time-to-local-date-time (utime &optional (tz nil have-tz))
  (multiple-value-bind (ss mm hh d m y wd) (apply #'decode-universal-time utime (and have-tz (list tz)))
    (make-local-date-time-1 (logior (ash y 16) (ash m 8) d) wd
                            (make-local-time-1 (logior (ash hh 16) (ash mm 8) ss) 0))))

(defun local-date-time= (ob1 ob2)
  (or (eq ob1 ob2)
      (and (local-date= ob1 ob2)
           (local-time= (ldt-time ob1) (ldt-time ob2)))))

(defun local-date-time-hash (object)
  (sxhash (logxor (ldate-value object) (local-time-hash (ldt-time object)))))

(defun local-date-time< (ob1 ob2)
  (let ((d1 (ldate-value ob1)) (d2 (ldate-value ob2)))
    (cond
      ((< d1 d2) t)
      ((> d1 d2) nil)
      (t (local-time< (ldt-time ob1) (ldt-time ob2))))))

(defun local-date-time/= (ob1 ob2) (not (local-date-time= ob1 ob2)))
(defun local-date-time> (ob1 ob2) (local-date-time< ob2 ob1))
(defun local-date-time>= (ob1 ob2) (not (local-date-time< ob1 ob2)))
(defun local-date-time<= (ob1 ob2) (not (local-date-time< ob2 ob1)))


(defun remove-from-plist (plist &rest keys)
  (loop
     for (key value) on plist by #'cddr
     unless (member key keys)
       nconcing (list key value)))
       


(defparameter +default-date+ (make-local-date 1970 1 1))
(defparameter +default-time+ (make-local-time 12 0 0 0))
(defparameter +default-date-time+ (make-local-date-time 1970 1 1 12 0 0 0))

(defmethod local-date ((object local-date) &key)
  object)

(defmethod local-time ((object local-date) &key)
  object)

(defmethod local-date ((object local-date-time) &key)
  (make-local-date-1 (ldate-value object) (ldate-weekday object)))

(defmethod local-time ((object local-date-time) &key)
  (ldt-time object))

(defmethod local-date ((object integer) &key ((:timezone tz) nil have-tz))
  (multiple-value-bind (u1 u2 u3 d m y w) (apply #'decode-universal-time object (and have-tz (list tz)))
    (declare (ignore u1 u2 u3))
    (make-local-date-1 (logior (ash y 16) (ash m 8) d) w)))

(defmethod local-time ((object integer) &key ((:timezone tz) nil have-tz))
  (multiple-value-bind (s m h) (apply #'decode-universal-time object (and have-tz (list tz)))
    (make-local-time-1 (logior (ash h 16) (ash m 8) s) 0)))

(defmethod local-date-time ((object integer) &key ((:timezone tz) nil have-tz))
  (multiple-value-bind (ss mm hh d m y w) (apply #'decode-universal-time object (and have-tz (list tz)))
    (make-local-date-time-1 (logior (ash y 16) (ash m 8) d) w
                            (make-local-time-1 (logior (ash hh 16) (ash mm 8) ss) 0))))

(defmethod local-date-time ((object local-date-time) &key)
  object)

(defmethod local-date-time ((object local-date)
                            &rest options
                            &key (defaults +default-date-time+))
  (make-local-date-time-1 (ldate-value object) (ldate-weekday object)
                          (apply #'local-time defaults (remove-from-plist options :defaults))))

(defmethod local-date-time ((object local-time)
                            &rest options
                            &key (defaults +default-date-time+))
  (let ((d (apply #'local-date defaults (remove-from-plist options :defaults))))
    (make-local-date-time-1 (ldate-value d) (ldate-weekday d) object)))

(defmethod local-date (object &key)
  (error 'type-error :datum object :expected-type 'local-date))

(defmethod local-time (object &key)
  (error 'type-error :datum object :expected-type 'local-time))

(defmethod local-date-time (object &key)
  (error 'type-error :datum object :expected-type 'local-date-time))



(defun print-local-date (object &optional (stream *standard-output*))
  (format stream "~D-~2,'0D-~2,'0D"
          (local-year object)
          (local-month object)
          (local-day object))
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




(defun first-of-month (object)
  (let ((date (local-date object)))
    (if (eql 1 (local-date-day date))
        date
        (make-local-date (local-date-year date) (local-date-month date) 1))))

(defun last-of-month (object)
  (let* ((date (local-date object))
         (year (local-date-year date))
         (month (local-date-month date))
         (day (local-date-day date))
         (max-day (days-in-month year month)))
    (if (eql max-day day) date
        (make-local-date year month max-day))))

(defun previous-month (object &key day)
  (let* ((date (local-date object))
         (year (local-date-year date))
         (month (local-date-month date))
         (dday (local-date-day date)))
    (multiple-value-bind (new-year new-month)
        (if (eql 1 month)
            (values (1- year) 12)
            (values year (1- month)))
      (let ((max (days-in-month new-year new-month)))
        (make-local-date new-year new-month
                         (cond
                           ((eq day :last) max)
                           ((eq day :first) 1)
                           ((null day) (min dday max))
                           (t day)))))))

(defun next-month (object &key day)
  (let* ((date (local-date object))
         (year (local-date-year date))
         (month (local-date-month date))
         (dday (local-date-day date)))
    (multiple-value-bind (new-year new-month)
        (if (eql 12 month)
            (values (1+ year) 1)
            (values year (1+ month)))
      (let ((max (days-in-month new-year new-month)))
        (make-local-date new-year new-month
                         (cond
                           ((eq day :last) max)
                           ((eq day :first) 1)
                           ((null day) (min dday max))
                           (t day)))))))




(defgeneric plus-days (object days &key))

(defmethod plus-days ((object local-date) (days integer) &key)
  (let* ((old-u (local-date-to-universal-time object 0))
         (new-u (+ old-u (* days 24 60 60))))
    (universal-time-to-local-date new-u 0)))

(defmethod plus-days ((object local-date-time) (days integer) &key ((:timezone tz) nil have-tz))
  (let* ((old-u (apply #'local-date-time-to-universal-time object (and have-tz (list tz))))
         (new-u (+ old-u (* days 24 60 60))))
    (apply #'local-date-time new-u (and have-tz (list :timezone tz)))))
