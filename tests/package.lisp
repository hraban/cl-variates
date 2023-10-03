(in-package #:common-lisp-user)

(defpackage #:cl-variates-test
  (:use #:common-lisp #:lift #:cl-variates)
  (:shadowing-import-from #:cl-variates #:random-element))
