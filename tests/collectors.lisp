(defpackage :collectors-test
  (:use :cl :cl-user :collectors :lisp-unit))

(in-package :collectors-test)

(define-test make-reducer
  (let ((r (make-reducer #'+ 0)))
    (funcall r 0)
    (funcall r 1 2)
    (funcall r 1 2 3)
    (assert-eql 9 (funcall r))))

(define-test with-reducer
  (with-reducer (r #'+ 0)
    (r 0)
    (r 1 2)
    (r 1 2 3)
    (assert-eql 9 (r))))

(define-test with-collector
  (with-collector (test)
    (test :a :key)
    (test :and :a)
    (test :value :make)
    (test :a :plist)
    (assert-equal
        '(:a :key :and :a :value :make :a :plist)
        (test)
     )))

(define-test with-appender
  (with-appender (test)
    (test :a :key)
    (test '(:and :a))
    (test '(:value) '(:make))
    (test '(:a :plist))
    (assert-equal
        '(:a :key :and :a :value :make :a :plist)
        (test)
     )))

(define-test with-string-builder
  (with-string-builder (test)
    (test :a :key)
    (test :and :a)
    (test :value :make)
    (test "" nil)
    (test :a :plist)
    (assert-equal
        "AKEYANDAVALUEMAKEAPLIST"
        (test)
     ))
  (with-string-builder (test :delimiter ", ")
    (test nil)
    (test :put)
    (test nil)
    (test :some)
    (test :commas)
    (test :in)
    (assert-equal
        "PUT, SOME, COMMAS, IN"
        (test)
     )))

(run-tests)
