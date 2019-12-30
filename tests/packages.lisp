;;;; packages.lisp --- test package definition

;;;; Code:

(in-package :cl-user)

(defpackage :commonmark-tests
  (:use :cl :commonmark :5am))

(in-package :commonmark-tests)

(def-suite commonmark
    :description "All cl-commonmark tests")

;;;; packages.lisp ends here
