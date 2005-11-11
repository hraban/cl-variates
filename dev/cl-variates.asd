;;; -*- Mode: Lisp; package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

#|

|#

(in-package asdf)
(defpackage "ASDF-CL-VARIATES" (:use #:cl #:asdf))
(in-package "ASDF-CL-VARIATES")

(defsystem :variates 
  :version "0.8"
  :author "Gary King <gwking@cs.umass.edu>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "A generic container library for Common Lisp"

  :components ((:file "package")
               (:file "variates" 
                      :depends-on ("package")))
  :depends-on (cl-mathstat))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
