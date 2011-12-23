# Collectors

A small collection of common lisp macros to make collecting values
easier.

## Origin Info

Much of this code was originally in arnesi, but I often want to use
these in code that doesnt need to require all of arnesi.  Also arnesi 
is hard to update.

[original arnesi docs](http://common-lisp.net/project/bese/docs/arnesi/html/Reducing_0020and_0020Collecting.html)
 

## API 

### make-collector / with-collector / with-mapping-collector

Create a collector function.

A Collector function will collect, into a list, all the values
passed to it in the order in which they were passed. If the
callector function is called without arguments it returns the
current list of values. 

```
   (with-collector (col)
       (col 1)
       (col 2)
       (col 3)
       (col)) => (1 2 3)
```

Mapping collectors mutate the collected value while collecting it.

```
   (with-mapping-collector (col (x) (* 2 x))
       (col 1)
       (col 2)
       (col 3)
       (col)) => (2 4 6)
```


### make-reducer / with-reducer

Create a function which, starting with INITIAL-VALUE, reduces
any other values into a single final value.

FUNCTION will be called with two values: the current value and
the new value, in that order. FUNCTION should return exactly one
value.

The reducing function can be called with n arguments which will
be applied to FUNCTION one after the other (left to right) and
will return the new value.

If the reducing function is called with no arguments it will
return the current value.

```
Example:
 (setf r (make-reducer #'+ 5))
 (funcall r 0) => 5
 (funcall r 1 2) => 8
 (funcall r) => 8
```


### make-appender / with-appender / with-mapping-appender

Create an appender function.

An Appender will append any arguments into a list, all the values
passed to it in the order in which they were passed. If the
appender function is called without arguments it returns the
current list of values.

```
    (with-appender (app)
       (app '(1 2))
       (app '(2 3))
       (app '(3 4))
       (app)) => (1 2 2 3 3 4)
```

Mapping appenders mutate the collected values while collecting them.

```
   (with-mapping-appender (app (l) (mapcar #'(lambda (x) (* 2 x)) l))
       (app '(1 2))
       (app '(2 3))
       (app '(3 4))
       (app)) => (2 4 4 6 6 8)
```


### make-string-builder / with-string-builder / with-string-builder-output

Create a function that will build up a string for you Each call to the
function with arguments appends those arguments to the string with an
optional delimiter between them.

if ignore-empty-strings-and-nil is true neither empty strings nor nil
will be printed to the stream

A call to the function with no arguments returns the output string

with-string-builder-output returns the collected string as the value
of the "with" form

## Authors

* Marco Baringer - Author of Arnesi
* [Acceleration.net](http://www.acceleration.net/) - [Donate](http://www.acceleration.net/programming/donate-to-acceleration-net/)
** [Russ Tyndall](http://russ.unwashedmeme.com/blog)
** [Nathan Bird](http://the.unwashedmeme.com/blog)
** [Ryan Davis](http://ryepup.unwashedmeme.com/blog)

```
;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
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
```
