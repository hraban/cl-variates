;;;-*- Mode: Lisp; Package: VARIATES -*-

#| simple-header

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