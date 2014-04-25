# Collectors

A small collection of common lisp macros to make collecting values
easier.

## Origin Info

Much of this code was originally in arnesi, but I often want to use
these in code that doesnt need to require all of arnesi.  Also arnesi 
is hard to update.

[original arnesi docs](http://common-lisp.net/project/bese/docs/arnesi/html/Reducing_0020and_0020Collecting.html)

At this point the API and performance profile has diverged from ARNESI

## API 

### Definitions - as applied to this library

Aggregators: functions / objects which take many values and combine
   them in some logical way.  All aggregators accept `N` values and,
   after aggregating each of them, return the new aggregate value

Reducers: aggregate values by combining new values into a single value,
   eg: #'+ can be used to reduce successive numbers into their sum

Collectors: aggregate items into a list by adding each item to the end
   of a list.

Appenders: aggregate items into a list by appending each item to the
   end. If single, the item is collected, if a list, it is appended 
   eg: (app (1) 2 (3 (4))) => (1 2 3 (4))

Pusher: collect items (by push) at the beginning of a list

### Simple aggregators

Simple aggregators are lambdas which do not signal (are not
filterable), and store their data in local places.

#### append-at-end, collect-at-end

Macros for inlining collection at the end of a list by providing
places for each operation

#### make-simple-collector / make-simple-appender

Quickest possible function based implementation of a collector /
appender. Returns (lambda (&rest values) {do} collected-values),
The `-to-place` variants are macros that build the same function, 
but with the head of the list stored in a user-provided place


### Nonsimple aggregators

Non simple aggregators are funcallable CLOS instances with a type
heirachy rooted at `value-aggregator`.

These type of aggregators support a standard set of operations and
signals.  

#### `operate` / `deoperate`

These methods perform the correct operation for the type of aggregator
(EG: reducing, appending, collecting, pushing).  Deoperate attempts to
undo the operation (currently only defined for the list types).

#### `should-aggregate?`

Given an aggregator and a value, should we include the value in our
collection.  Used in conjunction with the skip restart to orchestrate
skipping items.

#### Place Setters

This is function (or list thereof) that writes the aggregate value to the place after each 

#### Signals and Restarts

* collectors-signals:aggregating - signaled when we begin aggregating a value
* collectors-signals:done-aggregating - signaled when we finish aggregating a value
* collectors-signals:skip - *restart* skip aggregating this item (used to filter out nils etc)
* collectors-signals:use-value - *restart* aggregate a different value instead (used for
  mapping)

#### make-collector, make-pusher, make-reducer, make-appender

Creates a funcallable instance to perform the expected operation



#### Strings: make-formatter (fn), string-formatter (class), with-formatter (macro)

String formatters accept a format-string and list of arguments, and
use format to process the two into a string.  If a stream is provided
we write that string to the stream.  We also always concatenate the
results of all formatter calls.  The function (as all aggregators)
returns the concatenated results of all calls.

Optionally a delimiter will be written between each call to the
formatter.

A provided stream will be written to as each formatter call is made.

#### Strings: make-string-builder / with-string-builder / with-string-builder-output

Create a function that will build up a string for you. Each call to
the function concatenates all arguments (coerced to string via princ)
into the result.

if ignore-empty-strings-and-nil is true neither empty strings nor nil
will be collect to the stream / aggregate.  (Delimiters will also be
elided)

Optionally a delimiter will be written between each call to the
formatter.

A provided stream will be written to as each formatter call is made.

### Context Macros `with-collector` & `with-collector-output`

Create a lexical function that calls a new aggregator of the requested
type.  When using the `-output` variants, the aggregate value is
returned from the form. Otherwise, the value of the last form is
returned per-usual.

```
   (with-collector (col)
       (col 1) ; (1)
       (col 2) ; (1 2)
       (col 3) ; (1 2 3)
       (col)) => (1 2 3)
```


## Authors

 * Marco Baringer - Author of Arnesi
 * [Acceleration.net](http://www.acceleration.net/) - [Donate](http://www.acceleration.net/programming/donate-to-acceleration-net/)
  * [Russ Tyndall](http://russ.unwashedmeme.com/blog)
  * [Nathan Bird](http://the.unwashedmeme.com/blog)
  * [Ryan Davis](http://ryepup.unwashedmeme.com/blog)

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
