(defpackage :parse-lisp-spec
  (:use :cl)
  (:documentation "Parse the dpans files to a Lisp structure"))

(in-package :parse-lisp-spec)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
