;;;-*- Mode: Lisp; Package: VARIATES -*-

#| simple-header

$Id: variates-thread-safe.lisp,v 1.1 2005/04/18 13:43:29 gwking Exp $

Copyright 1992 - 2004 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King

DISCUSSION

|#
(in-package variates)

(defclass thread-safe-random-number-generator ()
  ())

;;; ---------------------------------------------------------------------------

(defmethod make-generator ((class list) (seed number) &key)
  (let ((class-name (find-matching-rng-class class)))
    (make-instance class-name :random-seed seed)))

;;; ---------------------------------------------------------------------------

(defun find-matching-rng-class (classes)
  (or (u:find-existing-subclass 'basic-random-number-generator classes)
      (u:define-class (u:simple-define-class-name classes)
        classes nil)))

;;; ---------------------------------------------------------------------------

(defmethod next-element :around ((rng thread-safe-random-number-generator))
  (declare (optimize speed))
  (u:without-interrupts (call-next-method)))



;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************