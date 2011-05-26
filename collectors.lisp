;; -*- lisp -*-

(cl:defpackage :collectors
  (:use :cl :cl-user)
  (:export
   #:with-collector
   #:with-collectors
   #:make-collector
   #:make-pusher
   #:with-reducer
   #:make-reducer
   #:with-appender
   #:make-appender
   #:with-string-builder
   #:make-string-builder
   #:with-mapping-collector
   #:with-mapping-appender
   ))

(in-package :collectors)

(defun make-string-builder (&optional
                            delimiter
                            (ignore-empty-strings-and-nil t)
                            (pretty nil))
  "Create a function that will build up a string for you
   Each call to the function with arguments appends those arguments to the string
   with an optional delimiter between them.

   if ignore-empty-strings-and-nil is true neither empty strings nor nil will be
   printed to the stream

   A call to the function with no arguments returns the output string"
  (let ((s (make-string-output-stream))
        (*print-pretty* pretty)
        (printed? nil))
    (setf delimiter
          (typecase delimiter
            ((or null string) delimiter)
            (t  (princ-to-string delimiter))))
    (flet ((p (item)
             (when (or (null ignore-empty-strings-and-nil)
                       item)
               (when (and printed? delimiter)
                 (write-sequence delimiter s))
               (typecase item
                 (string (when (or (null ignore-empty-strings-and-nil)
                                   (plusp (length item)))
                           (write-sequence item s)))
                 (T (princ item s)))
               (setf printed? t))))
      (lambda (&rest args)
        (if args
            (mapc #'p args)
            (get-output-stream-string s))))))

(defmacro with-string-builder ((name &key delimiter (ignore-empty-strings-and-nil t))
                               &body body)
  "A macro that creates a string builder with name in scope during the
   duration of the env"
  (alexandria:with-unique-names (it)
    `(let ((,it (make-string-builder ,delimiter ,ignore-empty-strings-and-nil)))
      (flet ((,name (&rest items) (apply ,it items)))
        ,@body))))

;;;; * Reducing and Collecting

;;;; ** Reducing

;;;; reducing is the act of taking values, two at a time, and
;;;; combining them, with the aid of a reducing function, into a
;;;; single final value.

(defun make-reducer (function &optional (initial-value nil initial-value-p))
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
  (let ((value initial-value))
    (lambda (&rest next)
      (when next
        ;; supplied a value, reduce
        (if initial-value-p
            ;; have a value to test against
            (dolist (n next)
              (setf value (funcall function value n)))
            ;; nothing to test againts yet
            (setf initial-value-p t
                  value next)))
      ;; didn't supply a value, return the current value
      value)))

(defmacro with-reducer ((name function &optional (initial-value nil))
                        &body body)
  "Locally bind NAME to a reducing function. The arguments
FUNCTION and INITIAL-VALUE are passed directly to MAKE-REDUCER."
  (alexandria:with-unique-names (reducer)
    `(let ((,reducer (make-reducer ,function ,@(list initial-value))))
       (flet ((,name (&rest items)
                (if items
                    (dolist (i items)
                      (funcall ,reducer i))
                    (funcall ,reducer))))
         ,@body))))

;;;; ** Collecting
;;;;
;;;; Building up a list from multiple values.

(defun make-collector (&optional initial-value (collect-nil t))
  "Create a collector function.

A Collector function will collect, into a list, all the values
passed to it in the order in which they were passed. If the
callector function is called without arguments it returns the
current list of values."
  (let ((value initial-value)
        (cdr (last initial-value)))
    (lambda (&rest items)
      (unless collect-nil (setf items (delete-if #'null items)))
      (if items
          (progn
            (if value
                (if cdr
                    (setf (cdr cdr) items
                          cdr (last items))
                    (setf cdr (last items)))
                (setf value items
                      cdr (last items)))
            items)
          value))))

(defun make-appender (&optional initial-value)
  "Create an appender function.

An Appender will append any arguments into a list, all the values
passed to it in the order in which they were passed. If the
appender function is called without arguments it returns the
current list of values."
  (let ((collector (make-collector initial-value)))
    (lambda (&rest items)
      ;; flatten one level and append lists for appender
      (setf items (apply #'append
		   (mapcar #'alexandria:ensure-list items)))
      (apply collector items))))

(defun make-pusher (&optional initial-value (collect-nil t))
  "Create a function which collects values as by PUSH."
  (let ((value initial-value))
    (lambda (&rest items)
      (if items
          (progn
            (dolist (i items)
              (when (or collect-nil i)
                (push i value)))
            items)
          value))))

(defmacro with-appender ((name &optional initial-value) &body body)
  "Bind NAME to a collector function and execute BODY. If
  FROM-END is true the collector will actually be a pusher, (see
  MAKE-PUSHER), otherwise NAME will be bound to a collector,
  (see MAKE-COLLECTOR)."
  (alexandria:with-unique-names (appender)
    `(let ((,appender (make-appender ,initial-value)))
       (flet ((,name (&rest items)
		(apply ,appender items)))
         ,@body))))

(defmacro with-collector ((name &key
                            (collect-nil T)
                            initial-value from-end) &body body)
  "Bind NAME to a collector function and execute BODY. If
  FROM-END is true the collector will actually be a pusher, (see
  MAKE-PUSHER), otherwise NAME will be bound to a collector,
  (see MAKE-COLLECTOR)."
  (alexandria:with-unique-names (collector)
    `(let ((,collector ,(if from-end
                            `(make-pusher ,initial-value ,collect-nil)
                            `(make-collector ,initial-value ,collect-nil))))
       (flet ((,name (&rest items)
		(apply ,collector items)))
         ,@body))))

(defmacro with-collectors (names &body body)
  "Bind multiple collectors. Each element of NAMES should be a
  list as per WITH-COLLECTOR's first orgument."
  (if names
      `(with-collector ,(alexandria:ensure-list (car names))
         (with-collectors ,(cdr names) ,@body))
      `(progn ,@body)))

;;;; Mapping collectors
(defmacro with-mapping-collector ((name fn-args &body fn-body)
                                  &body body)
  "Like a with-collector, but instead of a name we take a function spec

   if you call the resultant function with no arguments, you get the
     collection so far
   if you call it with arguments the results of calling your function spec are
     collected "
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
     collected "
  (alexandria:with-unique-names (col flet-args)
    `(let ((,col (make-appender)))
      (flet ((,name (&rest ,flet-args)
               (if ,flet-args
                   (funcall ,col (apply (lambda ,fn-args ,@fn-body)
                                        ,flet-args))
                   (funcall ,col))))
        ,@body))))

;; Copyright (c) 2002-2006, Edward Marco Baringer
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
