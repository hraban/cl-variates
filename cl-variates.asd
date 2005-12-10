;;; -*- Mode: Lisp; package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

#|

|#

(in-package asdf)
(defpackage "ASDF-CL-VARIATES" (:use #:cl #:asdf))
(in-package "ASDF-CL-VARIATES")

(defsystem cl-variates 
  :version "0.8"
  :author "Gary King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "A generic container library for Common Lisp"

  :components ((:module "dev"
                        :components ((:file "package")
                                     (:file "variates" 
                                            :depends-on ("package")))))
  :depends-on (cl-mathstats))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************