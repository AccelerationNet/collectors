;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :collectors.system)
    (defpackage :collectors.system
	(:use :common-lisp :asdf))))

(in-package collectors.system)

(defsystem :collectors
  :description "A library providing various collector type macros
   pulled from arnesi into its own library and stripped of dependencies"
  :licence "BSD"
  :version "0.1"
  :author "Marco Baringer, Russ Tyndall <russ@acceleration.net>"
  :maintainer "Russ Tyndall <russ@acceleration.net>"
  :components ((:file "collectors"))
  :depends-on (:alexandria :closer-mop :symbol-munger))

(defsystem :collectors-test
  :description "A library providing various collector type macros
   pulled from arnesi into its own library"
  :licence "BSD"
  :version "0.1"
  :author "Marco Baringer, Russ Tyndall <russ@acceleration.net>"
  :maintainer "Russ Tyndall <russ@acceleration.net>"
  :components ((:module :tests
			:serial t
			:components ((:file "collectors"))))
  :depends-on (:collectors :lisp-unit2))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :collectors))))
  (asdf:load-system :collectors-test)
  (let ((*package* (find-package :collectors-test)))
    (eval (read-from-string "
      (run-tests :package :collectors-test
                 :name :collectors
                 :run-contexts #'with-summary-context)"))))

;; Copyright (c) 2002-2006, Edward Marco Baringer (from arnesi lib)
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
