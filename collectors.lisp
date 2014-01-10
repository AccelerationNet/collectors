;; -*- lisp -*-

(cl:defpackage :collectors
  (:use :cl :cl-user)
  (:export
   #:with-collector
   #:with-collector-output
   #:with-collectors
   #:make-collector
   #:make-pusher
   #:with-reducer
   #:make-reducer
   #:with-appender
   #:with-appender-output
   #:make-appender
   #:with-string-builder
   #:with-string-builder-output
   #:make-string-builder
   #:with-mapping-collector
   #:with-mapping-appender
   #:make-formatter
   #:with-formatter
   #:with-formatter-output
   ))

(in-package :collectors)

;;;; * Reducing and Collecting

;;;; ** Reducing

;;;; reducing is the act of taking values, two at a time, and
;;;; combining them, with the aid of a reducing function, into a
;;;; single final value.

(defun make-reducer (function &key initial-value place-setter)
  "Create a function which, starting with INITIAL-VALUE, reduces
any other values into a single final value.

FUNCTION will be called with two values: the current value and
the new value, in that order. FUNCTION should return exactly one
value.

The reducing function can be called with n arguments which will
be applied to FUNCTION one after the other (left to right) and
will return the new value.

If the reducing function is called with no arguments it will
return the current value.

Example:

 (setf r (make-reducer #'+ 5))
 (funcall r 0) => 5
 (funcall r 1 2) => 8
 (funcall r) => 8"
  (let ((reduction initial-value))
    (lambda (&rest next)
      (dolist (n next)
        (setf reduction
              (if (null reduction)
                  n
                  (funcall function reduction n))))
      (when place-setter (funcall place-setter reduction))
      reduction)))

(defmacro with-reducer ((name function &key (initial-value nil) place)
                        &body body)
  "Locally bind NAME to a reducing function. The arguments
FUNCTION and INITIAL-VALUE are passed directly to MAKE-REDUCER."
  (alexandria:with-unique-names (reducer)
    `(let ((,reducer (make-reducer ,function
                      :initial-value ,initial-value
                      :place-setter ,(when place `(lambda (new) (setf ,place new))))))
      (flet ((,name (&rest items) (apply ,reducer items)))
        ,@body))))

;;;; ** Collecting
;;;;
;;;; Building up a list from multiple values.

(defun make-collector (&key initial-value (collect-nil t) place-setter)
  "Create a collector function.

A Collector function will collect, into a list, all the values
passed to it in the order in which they were passed. If the
callector function is called without arguments it returns the
current list of values."
  ;; by using this head cons cell we can simplify the loop body
  (let* ((head (cons :head (copy-list initial-value)))
         (cdr (last head)))
    (lambda (&rest items)
      (declare (dynamic-extent items))
      (loop for i in items
            when (or collect-nil i)
            do (let ((new-cons (cons i nil)))
                 (setf (cdr cdr) new-cons
                       cdr new-cons)))
      (when place-setter (funcall place-setter (cdr head)))
      (cdr head)
      )))

(defun make-appender (&key initial-value place-setter)
  "Create an appender function.

An Appender will append any arguments into a list, all the values
passed to it in the order in which they were passed. If the
appender function is called without arguments it returns the
current list of values."
  (let ((collector (make-collector :initial-value initial-value :place-setter place-setter)))
    (lambda (&rest items)
      ;; flatten one level and append lists for appender
      (setf items (apply #'append
                         (mapcar #'alexandria:ensure-list items)))
      (apply collector items))))

(defun make-pusher (&key initial-value (collect-nil t) place-setter)
  "Create a function which collects values as by PUSH."
  (let ((collection (copy-list initial-value)))
    (lambda (&rest items)
      (declare (dynamic-extent items))
      (dolist (i items)
        (when (or collect-nil i)
          (push i collection)))
      (when place-setter (funcall place-setter collection))
      collection)))

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
                       :initial-value ,initial-value
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
  (alexandria:with-unique-names (collector)
    `(let ((,collector (,(if from-end
                             'make-pusher
                             'make-collector)
                        :initial-value ,initial-value
                        :collect-nil ,collect-nil
                        :place-setter ,(when place `(lambda (new) (setf ,place new))))))
      (flet ((,name (&rest items)
               (apply ,collector items)))
        ,@body))))

(defmacro with-collector-output ((name &key (collect-nil t) initial-value from-end place)
                                 &body body)
  `(with-collector (,name :collect-nil ,collect-nil
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

(defun make-formatter (&key delimiter stream pretty)
  "Create a string formatter collector function.

creates a (lambda &optional format-string &rest args) and collects these in a list
When called with no args, returns the concatenated (with delimiter) results

binds *print-pretty* to nil
"
  (let* ((*print-pretty* pretty)
         (format-string (if delimiter
                            (format nil "~~{~~?~~^~a~~}" delimiter)
                            "~{~?~}")))
    (with-collector (value)
      (lambda (&rest args)
        (if (null args)
            (let ((*print-pretty* pretty))
              (format stream format-string (value)))
            (destructuring-bind (format-string &rest args) args
              (value format-string args)))))))

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

(defun make-string-builder (&key
                              delimiter
                              (ignore-empty-strings-and-nil t)
                              pretty
                              stream)
  "Create a function that will build up a string for you
   Each call to the function with arguments appends those arguments to the string
   with an optional delimiter between them.

   if ignore-empty-strings-and-nil is true neither empty strings nor nil will be
   printed to the stream

   A call to the function with no arguments returns the output string"
  (let ((formatter (make-formatter :delimiter delimiter :pretty pretty :stream stream))
        (include-empty? (not ignore-empty-strings-and-nil)))
    (flet ((include-arg? (arg)
             (or include-empty?
                 (if (stringp arg)
                     (plusp (length arg))
                     arg))))
      (lambda (&rest args)
        (declare (dynamic-extent args))
        (if args
            (loop for arg in args
                  when (include-arg? arg)
                    do (apply formatter (list "~A" arg)))
            (funcall formatter))))))

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
      (flet ((,name (&rest ,flet-args)
               (if ,flet-args
                   (funcall ,col (apply (lambda ,fn-args ,@fn-body)
                                ,flet-args))
                   (funcall ,col))))
        ,@body))))

(defmacro with-mapping-appender ((name fn-args &body fn-body)
                                 &body body)
  "Like a with-appender, but instead of a name we take a function spec

   if you call the resultant function with no arguments, you get the
     collection so far
   if you call it with arguments the results of calling your function spec are
     collected
   (with-mapping-appender (app (l) (mapcar #'(lambda (x) (* 2 x)) l))
       (app '(1 2))
       (app '(2 3))
       (app '(3 4))
       (app)) => (2 4 4 6 6 8)
  "
  (alexandria:with-unique-names (col flet-args)
    `(let ((,col (make-appender)))
      (flet ((,name (&rest ,flet-args)
               (if ,flet-args
                   (funcall ,col (apply (lambda ,fn-args ,@fn-body)
                                        ,flet-args))
                   (funcall ,col))))
        ,@body))))

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
