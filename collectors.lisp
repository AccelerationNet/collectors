;; -*- lisp -*-

(cl:defpackage :collectors-signals
  (:export
   ;; signals and restarts
   #:aggregating #:skip #:new-value #:value #:aggregator #:after-values
   #:done-aggregating #:aggregate))

(cl:defpackage :collectors
  (:use :cl :cl-user :collectors-signals)
  (:export
   #:collect-at-end
   #:append-at-end
   #:make-simple-collector
   #:make-simple-appender
   #:make-simple-collector-to-place
   #:make-simple-appender-to-place

   #:with-collector
   #:with-collector-output
   #:with-collectors
   #:make-collector
   #:with-alist-output
   #:collecting
   #:make-pusher
   #:with-reducer
   #:make-reducer
   #:with-appender
   #:with-appender-output
   #:make-appender
   #:appending
   #:with-string-builder
   #:with-string-builder-output
   #:make-string-builder
   #:with-mapping-collector
   #:with-mapping-appender
   #:make-formatter
   #:with-formatter
   #:with-formatter-output
   #:operate
   #:deoperate
   ))

(in-package :collectors)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition collectors-signals:aggregating ()
    ((collectors-signals:value
         :accessor collectors-signals:value
         :initarg :value :initform nil)
     (collectors-signals:aggregator
      :accessor collectors-signals:aggregator
      :initarg :aggregator :initform nil)))

  (define-condition collectors-signals:done-aggregating ()
    ((collectors-signals:after-values
      :accessor collectors-signals:after-values
      :initarg :after-values :initform nil)
     (collectors-signals:aggregate
         :accessor collectors-signals:aggregate
         :initarg :aggregate :initform nil)
     (collectors-signals:aggregator
      :accessor collectors-signals:aggregator
      :initarg :aggregator :initform nil)))  

  (defmacro with-signal-context ((value after-values aggregator) &body body)
    (alexandria:with-unique-names (new-value)
      `(with-simple-restart
        (collectors-signals:skip "Skip aggregating ~A into ~A" ,value ,aggregator)
        (restart-case (signal 'aggregating :value ,value :aggregator ,aggregator)
          (collectors-signals:new-value (,new-value)
            :report "Aggregate a new value instead"
            (setf ,value ,new-value)))
        (prog1 (progn ,@body)
          (signal 'done-aggregating :after-values ,after-values :aggregator ,aggregator))))))

(defmacro collect-at-end (head-place tail-place values-place)
  "Macros to ease efficient collection at the end of a list"
  (alexandria:with-unique-names (a)
    `(dolist (,a ,values-place)
      (let ((c (cons ,a nil)))
        (when (null ,head-place) (setf ,head-place c))
        (unless (null ,tail-place) (setf (cdr ,tail-place) c))
        (setf ,tail-place c)))))

(defmacro collect-at-end-with-signals (head-place tail-place values-place
                                       aggregator-place post-values-place)
  "Macros to ease efficient collection at the end of a list"
  (alexandria:with-unique-names (a)
    `(dolist (,a ,values-place)
      (with-signal-context (,a ,post-values-place ,aggregator-place)
        (let ((c (cons ,a nil)))
          (when (null ,head-place) (setf ,head-place c))
          (unless (null ,tail-place) (setf (cdr ,tail-place) c))
          (setf ,tail-place c)
          )))))

(defmacro append-at-end (head-place tail-place values-place)
  "Macros to ease efficient collection (with appending) at the end of a list"
  (alexandria:with-unique-names (a)
    `(dolist (,a ,values-place)
      (typecase ,a
        (list (collect-at-end ,head-place ,tail-place ,a))
        (t (let ((c (cons ,a nil)))
             (when (null ,head-place) (setf ,head-place c))
             (unless (null ,tail-place) (setf (cdr ,tail-place) c))
             (setf ,tail-place (last c))))))))

(defmacro append-at-end-with-signals (head-place tail-place values-place
                                      aggregator-place post-values-place)
  "Macros to ease efficient collection (with appending) at the end of a list"
  (alexandria:with-unique-names (a)
    `(dolist (,a ,values-place)
      (with-signal-context (,a ,post-values-place ,aggregator-place)
        (typecase ,a
          (list (collect-at-end-with-signals
                 ,head-place ,tail-place ,a ,aggregator-place ,post-values-place))
          (t (let ((c (cons ,a nil)))
               (when (null ,head-place) (setf ,head-place c))
               (unless (null ,tail-place) (setf (cdr ,tail-place) c))
               (setf ,tail-place (last c)))))))))

(defmacro make-simple-collector-to-place (place)
  (alexandria:with-unique-names (tail)
    `(progn
      (setf ,place (alexandria:ensure-list ,place))
      (let* ((,tail (last ,place)))
        (lambda (&rest values)
          (collect-at-end ,place ,tail values)
          ,place)))))

(defun make-simple-collector (&optional initial-value)
  "A fastest possible, fewest frills collector suitable to places where efficiency matters"
  (let ((head initial-value))
    (make-simple-collector-to-place head)))

(defmacro make-simple-appender-to-place (place)
  "A fastest possible, fewest frills collector suitable to places where efficiency matters
   that appends any values that re lists"
  (alexandria:with-unique-names (tail)
    `(progn
      (setf ,place (alexandria:ensure-list ,place))
      (let ((,tail (last ,place)))
        (lambda (&rest values)
          (append-at-end ,place ,tail values)
          ,place)))))

(defun make-simple-appender (&optional initial-value)
  "A fastest possible, fewest frills collector suitable to places where efficiency matters
   that appends any values that re lists"
  (let ((head initial-value))
    (make-simple-appender-to-place head)))

(defclass value-aggregator (closer-mop:funcallable-standard-object)
  ((initial-value :accessor initial-value :initarg :initial-value :initform nil)
   (place-setter :accessor place-setter :initarg :place-setter :initform nil)
   (value :accessor value :initarg :value :initform nil))
  (:metaclass closer-mop:funcallable-standard-class))

(defclass list-aggregator (value-aggregator)
  ((collect-nil? :accessor collect-nil? :initarg :collect-nil? :initform t
    :documentation "Should we collect nil into our results")
   (new-only-test :accessor new-only-test :initarg :new-only-test :initform nil
    :documentation "If supplied with a new-only-test, we will verify that we
     have not already collected this item before collecting again")
   (new-only-key :accessor new-only-key :initarg :new-only-key :initform nil))
  (:metaclass closer-mop:funcallable-standard-class))

(defclass collector (list-aggregator)
  ((tail :accessor tail :initarg :tail :initform nil))
  (:documentation "Create a collector function.
   A Collector function will collect, into a list, all the values
   passed to it in the order in which they were passed. If the
   callector function is called without arguments it returns the
   current list of values.")
  (:metaclass closer-mop:funcallable-standard-class))

(defclass reducer (value-aggregator)
  ((operation :accessor operation :initarg :operation :initform nil))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Create a function which, starting with INITIAL-VALUE, reduces
any other values into a single final value.

OPERATION will be called with two values: the current value and
the new value, in that order. OPERATION should return exactly one
value.

The reducing function can be called with n arguments which will
be applied to OPERATION one after the other (left to right) and
will return the new value.

If the reducing function is called with no arguments it will
return the current value.

Example:

 (setf r (make-reducer #'+ 5))
 (funcall r 0) => 5
 (funcall r 1 2) => 8
 (funcall r) => 8"))

(defclass pusher (list-aggregator)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defclass appender (collector)
  ()
  (:documentation "Create an appender function.

   An Appender will append any arguments into a list, all the values
   passed to it in the order in which they were passed. If the
   appender function is called without arguments it returns the
   current list of values.")
  (:metaclass closer-mop:funcallable-standard-class))

(defclass string-formatter (value-aggregator)
  ((delimiter :accessor delimiter :initarg :delimiter :initform nil)
   (has-written? :accessor has-written? :initarg :has-written? :initform nil)
   (output-stream :accessor output-stream :initarg :output-stream :initform nil)
   (pretty? :accessor pretty? :initarg :pretty? :initform nil))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Create a string formatter collector function.

creates a (lambda &optional format-string &rest args) and collects these in a list
When called with no args, returns the concatenated (with delimiter) results

binds *print-pretty* to nil
"))

(defclass string-builder (string-formatter)
  ((ignore-empty-strings-and-nil?
    :accessor ignore-empty-strings-and-nil? :initarg :ignore-empty-strings-and-nil? :initform t))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Create a function that will build up a string for you
   Each call to the function with arguments appends those arguments to the string
   with an optional delimiter between them.

   if ignore-empty-strings-and-nil is true neither empty strings nor nil will be
   printed to the stream

   A call to the function with no arguments returns the output string"))

;;;; * Reducing and Collecting

;;;; ** Reducing

;; ACL was throwing errors about this not being finalized (seems odd) re github #3
;; https://github.com/AccelerationNet/collectors/issues/3
;; Fix it by: https://www.mail-archive.com/closer-devel@common-lisp.net/msg00169.html
(closer-mop:ensure-finalized (find-class 'closer-mop:standard-object))
(closer-mop:ensure-finalized (find-class 'closer-mop:standard-class))
(closer-mop:ensure-finalized (find-class 'closer-mop:funcallable-standard-object))
(closer-mop:ensure-finalized (find-class 'closer-mop:funcallable-standard-class))


(defmethod initialize-instance :after ((o value-aggregator) &key &allow-other-keys)
  (setf (value o) (typecase (initial-value o)
                    (list (copy-list (initial-value o)))
                    (t (initial-value o))))
  (closer-mop:set-funcallable-instance-function
   o (lambda (&rest values) (operate o values))))

(defgeneric should-aggregate? (aggregator value)
  (:method ((o value-aggregator) v) t)
  (:documentation "Should we aggregate a given value into our collection"))

(defgeneric deoperate (aggregator values &key test key)
  (:documentation "Undo the aggregation operation of an aggregator and list of values")
  (:method :after ((o value-aggregator) values &key test key
                   &aux (value (value o)))
    (declare (ignore values test key))
    (dolist (ps (alexandria:ensure-list (place-setter o)))
      (funcall ps value))))

(defgeneric operate (aggregator values)
  (:documentation "Perform the aggregation operation on the aggregator for the values")
  (:method :around ((o value-aggregator) values
                    &aux (places (alexandria:ensure-list (place-setter o))))
    (declare (ignore values))
    (handler-bind
        ((aggregating
           (lambda (c)
             (when (eql o (aggregator c))
               (unless (should-aggregate? (aggregator c) (value c))
                 (invoke-restart 'skip)))))
         (done-aggregating
           (lambda (c)
             (when (eql o (aggregator c))
               (dolist (p places)
                 (funcall p (value o)))))))
      (call-next-method)
      (value o))))


(defmethod operate ((o reducer) values)
  (dolist (v (alexandria:ensure-list values))
    (with-signal-context (v (value o) o)
      (setf (value o)
            (if (value o)
                (funcall (operation o) (value o) v)
                v)))))

;;;; reducing is the act of taking values, two at a time, and
;;;; combining them, with the aid of a reducing function, into a
;;;; single final value.

(defun make-reducer (function &key initial-value place-setter)
  (make-instance 'reducer :initial-value initial-value :place-setter place-setter
                 :operation function))

(defmacro with-reducer ((name function &key (initial-value nil) place)
                        &body body)
  "Locally bind NAME to a reducing function. The arguments
FUNCTION and INITIAL-VALUE are passed directly to MAKE-REDUCER."
  (alexandria:with-unique-names (reducer)
    `(let ((,reducer (make-reducer ,function
                      :initial-value (or ,initial-value ,place)
                      :place-setter ,(when place `(lambda (new) (setf ,place new))))))
      (flet ((,name (&rest items) (apply ,reducer items)))
        ,@body))))



(defmethod should-aggregate? ((o list-aggregator) v)
  (and (or (collect-nil? o) v)
       (or (null (new-only-test o))
           (null (member v (value o)
                         :test #'new-only-test
                         :key (or (new-only-key o) #'identity))))))

(defun make-pusher (&key initial-value collect-nil place-setter)
  (make-instance
   'pusher
   :initial-value initial-value
   :collect-nil? collect-nil
   :place-setter place-setter))

(defmethod %push ((o list-aggregator) values)
  (dolist (v (alexandria:ensure-list values))
    (with-signal-context (v (value o) o)
      (push v (value o))
      (when (and (typep o 'collector) (null (tail o)))
        (setf (tail o) (value o)))))
  (value o))

(defmethod %pop-n ((o list-aggregator) &optional (n 1))
  (let* ((head (value o))
         (len (length head)))
    (cond
      ((>= n len)
       (setf (value o) nil)
       (when (typep o 'collector)
         (setf (tail o) nil)))
      (t (let ((lastcons (nthcdr (- n 1) head)))
           (setf (value o) (cdr lastcons)
                 (cdr lastcons) nil))))
    (if (= 1 n)
        (car head)
        head)))

(defmethod %unenqueue-n ((o list-aggregator) &optional (n 1))
  (let* ((head (value o))
         (len (length head))
         (div (- len (+ 1 n)))
         (rtn (cond
                ((plusp div)
                 (let* ((c (nthcdr div head))
                        (rtn (cdr c)))
                   (setf (cdr c) nil)
                   (when (typep o 'collector)
                     (setf (tail o) c))
                   rtn))
                (t
                 (setf (value o) nil)
                 (when (typep o 'collector)
                   (setf (tail o) nil))
                 head))))
    (if (= 1 n)
        (car rtn)
        rtn)))

(defmethod %enqueue ((o list-aggregator) values
                     &aux (last (last (value o))))
  (collect-at-end-with-signals
   (value o) last values o (value o))
  (value o))

(defmethod %enqueue ((o collector) values)
  (collect-at-end-with-signals
   (value o) (tail o) values o (value o))
  (value o))

(defmethod operate ((o pusher) values)
  (%push o values))

(defmethod initialize-instance :after ((o collector) &key &allow-other-keys)
  (setf (tail o) (last (value o))))

(defmethod operate ((o collector) values)
  (%enqueue o values))

(defmethod deoperate ((o list-aggregator) to-remove
                      &key test key
                      &aux prev)
  (setf to-remove (alexandria:ensure-list to-remove))
  (loop for cons on (value o)
        for (this . next) = cons
        do (if (null (member (funcall (or key #'identity) this)
                             to-remove
                             :test (or test #'eql)))
               ;; not to remove
               (setf prev cons)
               (cond
                 ;; remove first elt
                 ((null prev)
                  (setf (value o) next))
                 ;; remove last elt
                 ((null next)
                  (setf (cdr prev) nil
                        (tail o) prev))
                 ;; remove from middle of the list
                 (t (setf (cdr prev) next)))))
  (value o))

(defun make-collector (&key initial-value (collect-nil t) place-setter)
  ;; by using this head cons cell we can simplify the loop body
  (make-instance 'collector
                 :initial-value initial-value :collect-nil? collect-nil
                 :place-setter place-setter))

(defmethod operate ((o appender) values)
  (append-at-end-with-signals (value o) (tail o) values o (values o)))

(defun make-appender (&key initial-value place-setter)
  (make-instance 'appender :initial-value initial-value :place-setter place-setter))

(defmacro with-appender ((name &key initial-value place) &body body)
  "Bind NAME to a collector function and execute BODY. If
  FROM-END is true the collector will actually be a pusher, (see
  MAKE-PUSHER), otherwise NAME will be bound to a collector,
  (see MAKE-COLLECTOR).

    (with-appender (app)
       (app '(1 2))
       (app '(2 3))
       (app '(3 4))
       (app)) => (1 2 2 3 3 4)

  "
  (alexandria:with-unique-names (appender)
    `(let ((,appender (make-appender
                       :initial-value (or ,initial-value ,place)
                       :place-setter ,(when place `(lambda (new) (setf ,place new))))))
       (flet ((,name (&rest items) (apply ,appender items)))
         ,@body))))

(defmacro with-appender-output ((name &key initial-value place) &body body)
  "Same as with-appender, but this form returns the collected values
   automatically
  "
  `(with-appender (,name :initial-value ,initial-value :place ,place)
    ,@body (,name)))

(defmacro with-collector ((name &key place (collect-nil T) initial-value from-end) &body body)
  "Bind NAME to a collector function and execute BODY. If
  FROM-END is true the collector will actually be a pusher, (see
  MAKE-PUSHER), otherwise NAME will be bound to a collector,
  (see MAKE-COLLECTOR).
    (with-collector (col)
       (col 1)
       (col 2)
       (col 3)
       (col)) => (1 2 3)
  "
  `(let ((,name (,(if from-end
                      'make-pusher
                      'make-collector)
                 :initial-value (or ,initial-value ,place)
                 :collect-nil ,collect-nil
                 :place-setter ,(when place `(lambda (new) (setf ,place new))))))
    (flet ((,name (&rest items) (operate ,name items))
           (,(symbol-munger:english->lisp-symbol `(push ,name))
               (&rest items)
             (%push ,name items))
           (,(symbol-munger:english->lisp-symbol `(pop ,name))
               (&optional (n 1))
             (%pop-n ,name n))
           (,(symbol-munger:english->lisp-symbol `(enqueue ,name))
               (&rest items)
             (%enqueue ,name items))
           (,(symbol-munger:english->lisp-symbol `(unenqueue ,name))
               (&optional (n 1))
             (%unenqueue-n ,name n))
           )
      ,@body)))

(defmacro with-collector-output ((name &key (collect-nil t) initial-value from-end place)
                                 &body body)
  `(with-collector (,name :collect-nil ,collect-nil
                          :initial-value ,initial-value
                          :from-end ,from-end
                          :place ,place)
    ,@body
    (,name)))

(defmacro with-alist ((name &key place (collect-nil T) initial-value from-end) &body body)
  `(let ((,name (,(if from-end
                      'make-pusher
                      'make-collector)
                 :initial-value (or ,initial-value ,place)
                 :collect-nil ,collect-nil
                 :place-setter ,(when place `(lambda (new) (setf ,place new))))))
    (flet ((,name (&rest items)
             (loop for (k v) on items by #'cddr
                   do (operate ,name (list (cons k v))))
             (value ,name)))
    ,@body)))

(defmacro with-alist-output ((name &key (collect-nil t) initial-value from-end place)
                                 &body body)
  `(with-alist (,name :collect-nil ,collect-nil
                      :initial-value ,initial-value
                      :from-end ,from-end
                      :place ,place)
    ,@body
    (,name)))



(defmacro with-collectors (names &body body)
  "Bind multiple collectors. Each element of NAMES should be a
  list as per WITH-COLLECTOR's first orgument."
  (if names
      `(with-collector ,(alexandria:ensure-list (car names))
         (with-collectors ,(cdr names) ,@body))
      `(progn ,@body)))


(defmethod initialize-instance :after ((o string-formatter) &key &allow-other-keys)
  (when (and (delimiter o) (not (stringp (delimiter o))))
    (setf (delimiter o) (princ-to-string (delimiter o))))
  (when (initial-value o)
    (setf (has-written? o) t)
    (when (output-stream o)
      (write-string (initial-value o) (output-stream o)))))

(defmethod operate ((o string-formatter) values)
  (setf values (alexandria:ensure-list values))
  (when values
    (let* ((*print-pretty* (pretty? o))
           (new-part (format nil "~@[~@?~]~?" (when (has-written? o) (delimiter o))
                             (first values) (rest values))))
      (setf (has-written? o) t)
      (setf (value o) (concatenate 'string (value o) new-part))
      (when (output-stream o)
        (write-string new-part (output-stream o))))))

(defun make-formatter (&key delimiter stream pretty)
  "Create a string formatter collector function.

   creates a (lambda &optional format-string &rest args) and collects these in a list
   When called with no args, returns the concatenated (with delimiter) results

   binds *print-pretty* to nil "
  (make-instance 'string-formatter :delimiter delimiter :output-stream stream :pretty? pretty ))

(defmacro with-formatter ((name &key delimiter stream pretty) &body body)
  "A macro makes a function with name for body that is a string formatter
see make-formatter"
  (alexandria:with-unique-names (fn-sym)
    `(let ((,fn-sym (make-formatter :delimiter ,delimiter :stream ,stream
                                    :pretty ,pretty)))
       (flet ((,name (&rest args) (apply ,fn-sym args)))
         ,@body))))

(defmacro with-formatter-output ((name &key delimiter stream pretty) &body body)
  "A macro makes a function with name for body that is a string formatter
see make-formatter.
This form returns the result of that formatter"
  `(with-formatter (,name :delimiter ,delimiter :stream ,stream :pretty ,pretty)
     ,@body
     (,name)))


(defmethod should-aggregate? ((o string-builder) v
                              &aux (collect-empty? (not (ignore-empty-strings-and-nil? o))))
  (or collect-empty?
      (and v (or (not (stringp v))
                 (plusp (length v))))))

(defmethod operate ((o string-builder) values
                    &aux
                    (delimiter (delimiter o))
                    (out (output-stream o))
                    (*print-pretty* (pretty? o)))
  (setf values (alexandria:ensure-list values))
  (dolist (v values)
    (with-signal-context (v (value o) o)
      (setf v (typecase v
                (string v)
                (t (princ-to-string v))))
      (cond
        ((and delimiter (has-written? o))
         (when out
           (write-string delimiter out)
           (write-string v out))
         (setf (value o) (concatenate 'string (value o) delimiter v)))
        ((has-written? o)
         (when out (write-string v out))
         (setf (value o) (concatenate 'string (value o) v)))
        (t
         (when out (write-string v out))
         (setf (value o) v)))
      (setf (has-written? o) t))))

(defun make-string-builder (&key delimiter ignore-empty-strings-and-nil stream)
  (make-instance 'string-builder
                 :output-stream stream
                 :delimiter delimiter
                 :ignore-empty-strings-and-nil? ignore-empty-strings-and-nil))

(defmacro with-string-builder ((name &key delimiter
                                     (ignore-empty-strings-and-nil t)
                                     stream)
                               &body body)
  "A macro that creates a string builder with name in scope during the
   duration of the env"
  (alexandria:with-unique-names (it items)
    `(let ((,it (make-string-builder
                 :delimiter ,delimiter
                 :ignore-empty-strings-and-nil ,ignore-empty-strings-and-nil
                 :stream ,stream)))
      (declare (type function ,it))
      (flet ((,name (&rest ,items)
               (declare (dynamic-extent ,items)) (apply ,it ,items)))
        ,@body))))

(defmacro with-string-builder-output ((name &key delimiter (ignore-empty-strings-and-nil t)
                                            stream)
                                           &body body)
  "A macro that creates a string builder with name in scope during the
   duration of the env, the form returns the string that is built"
  `(with-string-builder (,name :delimiter ,delimiter
                               :stream ,stream
                               :ignore-empty-strings-and-nil ,ignore-empty-strings-and-nil)
    ,@body
    (,name)))

(defun mapping-aggregation-context (body-fn &key aggregator map-fn)
  (handler-bind
      ((aggregating
         (lambda (c)
           (when (eql aggregator (aggregator c))
             (invoke-restart 'new-value (funcall map-fn (value c)))))))
    (funcall body-fn)))

(defmacro map-aggregation ((aggregator fn-spec) &body body)
  `(mapping-aggregation-context
    (lambda () ,@body)
    :aggregator ,aggregator
    :map-fn ,fn-spec))

;;;; Mapping collectors
(defmacro with-mapping-collector ((name fn-args &body fn-body)
                                  &body body)
  "Like a with-collector, but instead of a name we take a function spec

   if you call the resultant function with no arguments, you get the
     collection so far
   if you call it with arguments the results of calling your function spec are
     collected
   (with-mapping-collector (col (x) (* 2 x))
       (col 1)
       (col 2)
       (col 3)
       (col)) => (2 4 6)
   "
  (alexandria:with-unique-names (col flet-args)
    `(let ((,col (make-collector)))
      (map-aggregation (,col (lambda ,fn-args ,@fn-body))
        (flet ((,name (&rest ,flet-args) (apply ,col ,flet-args)))
          ,@body)))))

(defmacro with-mapping-appender ((name fn-args &body fn-body)
                                 &body body)
  "Like a with-appender, but instead of a name we take a function spec

   calling the function will appen
   
   (with-mapping-appender (app (l) (mapcar #'(lambda (x) (* 2 x)) l))
       (app '(1 2))
       (app '(2 3))
       (app '(3 4))
       (app)) => (2 4 4 6 6 8)
  "
  (alexandria:with-unique-names (col flet-args)
    `(let ((,col (make-appender)))
      (map-aggregation (,col (lambda ,fn-args ,@fn-body))
        (flet ((,name (&rest ,flet-args) (apply ,col ,flet-args)))
          ,@body)))))

(defmacro collecting ((arg list) &body body)
  "A mapping collecting macro for operating on elements of a list
   (similar to (mapcar (lambda (,arg) ,@body) list), but using a collector
    so all signals are in place)"
  `(with-collector-output (output)
    (dolist (,arg (alexandria:ensure-list ,list))
      (restart-case
          (output (progn ,@body))
        (skip () "Skip this element"))
      )))

(defmacro appending ((arg list) &body body)
  "A mapping collecting macro for operating on elements of a list
   (similar to (mapcan (lambda (,arg) ,@body) list), but using a collector
    so all signals are in place)"
  `(with-appender-output (output)
    (dolist (,arg (alexandria:ensure-list ,list))
      (restart-case
          (output (progn ,@body))
        (skip () "Skip this element")))))




;; Copyright (c) 2002-2006, Edward Marco Baringer
;;               2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
