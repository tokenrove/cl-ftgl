;;;; -*- Lisp -*-
;;;; cl-ftgl -- FTGL binding for CL.  See README for licensing information.

(defpackage #:cl-ftgl-system (:use #:cl #:asdf))
(in-package #:cl-ftgl-system)

(defsystem cl-ftgl
    :depends-on (:cffi :anaphora)
    :components
    ((:file "package")
     (:file "bindings" :depends-on ("package"))
     (:file "ftgl" :depends-on ("package" "bindings"))))
