;;;-*- Mode: Lisp; Package: VARIATES -*-

#| simple-header

$Id: variates-copying.lisp,v 1.1 2004/02/22 20:33:51 gwking Exp $

Copyright 1992 - 2004 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King

DISCUSSION

|#
(in-package variates)

;;; ---------------------------------------------------------------------------

(u:defcopy-methods basic-random-number-generator :set-all t)

(u:defcopy-methods ran1-random-number-generator :copy-all t)

(u:defcopy-methods ranq1-random-number-generator :copy-all t)

(u:defcopy-methods random-number-generation-mixin :copy-all t)


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************