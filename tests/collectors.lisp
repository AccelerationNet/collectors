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
    (test :a nil :key)
    (test :and :a)
    (test :value :make)
    (test :a :plist)
    (assert-equal
        '(:a nil :key :and :a :value :make :a :plist)
        (test)
     ))
  (with-collector (test :collect-nil nil)
    (test :a nil :key)
    (test :and :a)
    (test :value :make)
    (test :a :plist)
    (assert-equal
        '(:a :key :and :a :value :make :a :plist)
        (test)
     )))

(define-test with-collector2
  (with-collector (test :from-end t)
    (test :a nil :key)
    (test :and :a)
    (test :value :make)
    (test :a :plist)
    (assert-equal
        '(:plist :a :make :value :a :and :key nil :a)
        (test)
     ))
  (with-collector (test :from-end t :collect-nil nil)
    (test :a nil :key)
    (test :and :a)
    (test :value :make)
    (test :a :plist)
    (assert-equal
        '(:plist :a :make :value :a :and :key :a)
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

(define-test with-formatter
  (with-formatter (test)
    (test "~D ~D ~D" 0 0 0)
    (test "~A" 1)
    (test "~A" 2)
    (test "~A" 3)
    (test "ABC")
    (assert-equal "0 0 0123ABC" (test)))
  (with-formatter (test :delimiter "-")
    (test "~D ~D ~D" 0 0 0)
    (test "~A" 1)
    (test "~A" 2)
    (test "~A" 3)
    (test "ABC")
    (assert-equal "0 0 0-1-2-3-ABC" (test))))

(define-test with-mapping-collector
  (with-mapping-collector (test (&rest nums)
                            (apply #'+ nums))
    (test 1)
    (test 1 2)
    (test 1 2 3)
    (test 1 2 3 4)
    (assert-equal '(1 3 6 10) (test))))

(define-test with-mapping-appender
  (with-mapping-appender (test (&rest nums)
                            (mapcar (lambda (x) (* 2 x)) nums))
    (test 1)
    (assert-equal '(2) (test))
    (test 1 2)
    (assert-equal '(2 2 4) (test))
    (test 1 2 3)
    (test 1 2 3 4)
    (assert-equal '(2 2 4 2 4 6 2 4 6 8) (test))))

(run-tests)
