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

#|
(in-package u)

(defvar *test-rng* nil)

(defvar *threads* nil)

(defun make-rng-test-process (name)
  (make-thread name (lambda ()
                      (loop do
                            (next-element *test-rng*)))))

(defun test-multi-process-rng (count &key (class 'ran1-random-number-generator))
  (setf *test-rng* (make-random-number-generator 1 class))
  (setf *threads* 
        (loop repeat count 
              for i from 1 collect
              (make-rng-test-process (format nil "RNG-~D" i)))))

(defun stop-multi-process-rng ()
  (loop for thread in *threads* do (destroy-thread thread))
  (setf *threads* nil))

#+Test
(u::test-multi-process-rng 4)
#+Test
(u::test-multi-process-rng 
 4 
 :class '(ran1-random-number-generator thread-safe-random-number-generator))
#+Test
(stop-multi-process-rng)

#|
[billy-pilgrim:~/Repository/ijara-csif] gwking% find . \( -name "*.lisp" -and -exec grep -qi "make-random-number-generator" '{}' \; \) -print

### Ijara-csif
user-home:repository;ijara-csif;bugworld;dev;threat-defend-relations;pauls-debug-low-mem.lisp
user-home:repository;ijara-csif;bugworld;dev;threat-defend-relations;pauls-debug.lisp
user-home:repository;ijara-csif;eksl-math;dev/mersenne-twister.lisp
user-home:repository:ijara-csif:lbd/dev/cards.lisp
user-home:repository:ijara-csif:lbd/dev/lbd-game-engine.lisp
user-home:repository:ijara-csif:lbd/forward-backward/forward-backward-algorithm.lisp
user-home:repository:ijara-csif:opengl/opengl-tests/stencil-test.lisp
user-home:repository:ijara-csif:ttt/dev/experiments.lisp
user-home:repository:ijara-csif:utils/dev/geom/shapes.lisp
user-home:repository:ijara-csif:utils/dev/mersenne-twister.lisp
user-home:repository:ijara-csif:utils/dev/test-variates.lisp
user-home:repository:ijara-csif:variates/dev/mcl-variates.lisp
user-home:repository:ijara-csif:variates/dev/test-variates.lisp
user-home:repository:ijara-csif:variates/dev/variates.lisp

### CtF
user-home:repository:ijara-csif:afs/dev/afs-utils/package.lisp
user-home:repository:ijara-csif:afs/dev/domain/sensor-report.lisp
user-home:repository:ijara-csif:afs/dev/hpkb99/coa-parser-output.lisp
user-home:repository:ijara-csif:afs/dev/network/protocol.lisp
user-home:repository:ijara-csif:afs/dev/scenarios/basic-sidekick-scenario.lisp
user-home:repository:ijara-csif:OpenGL/opengl-tests/stencil-test.lisp
user-home:repository:ijara-csif:utils/dev/geom/shapes.lisp
user-home:repository:ijara-csif:utils/dev/mersenne-twister.lisp
user-home:repository:ijara-csif:utils/dev/test-variates.lisp
user-home:repository:ijara-csif:variates/dev/mcl-variates.lisp
user-home:repository:ijara-csif:variates/dev/test-variates.lisp
user-home:repository:ijara-csif:variates/dev/variates.lisp
|#
|#

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************