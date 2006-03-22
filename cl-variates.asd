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
  :description "Portable Common Lisp Random Number Generation and more."

  :components ((:module "dev"
                        :components ((:file "package")
                                     (:file "variates" 
                                            :depends-on ("package"))))
               (:module "website"
                        :components ((:module "source"
                                              :components ((:static-file "index.lml"))))))
  :depends-on (cl-mathstats))

(asdf:defsystem-connection variates-and-metacopy
  :requires (cl-variates metacopy)
  :components ((:module "dev"
                        :components ((:file "copying")))))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
