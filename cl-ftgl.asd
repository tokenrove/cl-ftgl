;; -*- Lisp -*-

(defpackage #:cl-ftgl-system (:use #:cl #:asdf))
(in-package #:cl-ftgl-system)

(defsystem cl-ftgl
    :depends-on (:cffi)
    :components
    ((:file "package")
     (:file "bindings" :depends-on ("package"))
     (:file "ftgl" :depends-on ("package" "bindings"))))
