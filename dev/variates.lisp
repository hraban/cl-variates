;;;-*- Mode: Lisp; Package: VARIATES -*-

#| simple-header

$Id: variates.lisp,v 1.19 2005/07/06 01:52:47 gwking Exp $

Copyright 1992 - 2004 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: EKSL

DISCUSSION

|#

#| TODO
copy-self so that we can make next-element faster
implement ran0, ran3, ran4, bitwise random numbers, etc.
|#

(in-package :variates)

;;; ---------------------------------------------------------------------------
;;; UNIFORM
;;; ---------------------------------------------------------------------------

(defun uniform-random (generator &optional (min 0.0d0) (max 1.0d0))
  "Returns a double-float pseudo random number between low (inclusive) 
and high (exclusive)."
  (let ((u (next-element generator)))
    (+ min (* u (- max min)))))

;;; ---------------------------------------------------------------------------

(defun integer-random (generator &optional (min 0) (max 1))
   "Returns a integer pseudo random number between low (inclusive) and high (inclusive)."
   (values (floor (uniform-random generator (float min 1d0) (float (1+ max) 1d0)))))

;;; ---------------------------------------------------------------------------
  
(defun random-range (generator low high)
  "Returns a pseudo random number between low (inclusive) and high (exclusive or inclusive depending are arguments).
If low and high are both integers (fixnums or bignums) this will return an integer, 
otherwise it will return a double-float."
  (if (and (integerp low)
           (integerp high))
    (values (integer-random generator low high))
    (values (uniform-random generator low high))))

;;; ---------------------------------------------------------------------------

(defun random-range-inclusive (generator low high)
  "Returns a pseudo random number between low and high (both inclusive).
If low and high are both integers (fixnums or bignums) this will return an integer, 
otherwise it will return a double-float."
  (if (and (integerp low)
           (integerp high))
    (values (floor (uniform-random generator low (1+ high))))
    (values (uniform-random generator low high))))

;;; ---------------------------------------------------------------------------

(defun random-boolean (generator &optional (probability 0.5d0))
  "Returns T with probability given. Defaults to 0.5d0."
  (when (plusp probability)
    (< (next-element generator) probability)))

;;; ---------------------------------------------------------------------------

(defun random-sample-with-range (generator size low high)
  (loop repeat size collect
        (random-range generator low high)))

;;; ---------------------------------------------------------------------------

(defun random-element (generator sequence &key (start 0) (end (1- (length sequence))))
  (if (or (and (consp sequence) (null sequence))
          (zerop (length sequence)))
    (values nil nil)
    (values
     (elt sequence (random-range generator start end))
     t)))

#+Alt
(defun random-element (generator sequence &key (start 0) (end (length sequence)))
  (if (or (and (consp sequence) (null sequence))
          (zerop (length sequence)))
    (values nil nil)
    (values
     (elt sequence (integer-random generator start end))
     t)))

;;; ---------------------------------------------------------------------------
;;; NORMAL
;;; ---------------------------------------------------------------------------

(defun normal-random (generator mean standard-deviation)
  "Returns a normally distributed double-float pseudo random number
with using 'mean' and 'standard-deviation'."
  (normal-random* generator mean standard-deviation))

;;; ---------------------------------------------------------------------------
;;; EXPONENTIAL
;;; ---------------------------------------------------------------------------

(defun exponential-random (generator &optional (rate 1.0d0))
  (exponential-random* generator rate))

;;; ---------------------------------------------------------------------------
;;; POSISSON
;;; ---------------------------------------------------------------------------

(defun poisson-random (generator mean)
  (poisson-random* generator (coerce mean 'double-float)))


;;; ---------------------------------------------------------------------------
;;; classes and api
;;; ---------------------------------------------------------------------------

(defgeneric (setf random-seed) (random-seed random-number-generator)
  (:documentation "Alters the seed of a pseudo random number generator."))

;;; ---------------------------------------------------------------------------

(defgeneric next-element (random-number-generator)
  (:documentation "Returns the next pseudo random number from a random number
generator (using the generator as output stream metaphor)."))

;;; ---------------------------------------------------------------------------

(defgeneric initialize-random-number-generator (random-number-generator)
  (:documentation "\(Internal\) \(Re\)initialize a random number generator. Called
when the generator is created and when the seed is changed."))

;;; ---------------------------------------------------------------------------

(defun make-random-number-generator (&optional (random-seed 42)
                                               (class 'ran1-random-number-generator))
  "Return a new random number generator of class `class' using `seed' as 
the initial seed." 
  (make-instance class :random-seed random-seed))

;;; ---------------------------------------------------------------------------

(defclass basic-random-number-generator ()
  ((random-seed :initarg :random-seed :reader random-seed))
  (:default-initargs 
    :random-seed 42)
  (:documentation "Root superclass for all random number generators."))

;;; ---------------------------------------------------------------------------

(defmethod (setf random-seed) ((random-seed number) (rng basic-random-number-generator))
  (when (zerop random-seed)
    (error "Seed must never be zero."))
  (setf (slot-value rng 'random-seed) random-seed)
  (initialize-random-number-generator rng)
  (values (random-seed rng)))


;;; ---------------------------------------------------------------------------
;;; ran1-random-number-generator (from numerical recipees)
;;; ---------------------------------------------------------------------------

(defclass ran1-random-number-generator (basic-random-number-generator)
  ((internal-seed :initform 0.0d0 :reader internal-seed :initarg :internal-seed)
   (ia :initform 16807.0d0 :reader ia :initarg :ia)
   (im :initform 2.147483647d+9 :reader im :initarg :im)
   (am :initform 0.0d0 :reader am :initarg :am)
   (iq :initform 127773.0d0 :reader iq :initarg :iq)
   (ir :initform 2836.0d0 :reader ir :initarg :ir)
   (ntab :initform 32 :reader ntab :initarg :ntab)
   (ndiv :initform 0.0d0 :reader ndiv :initarg :ndiv)
   (rnmx :initform 0.0d0 :reader rnmx :initarg :rnmx)
   (iy :initform 0.0d0 :reader iy :initarg :iy)
   (iv :initform :unbound :reader iv :initarg :iv))
  (:documentation 
   "From Numerical Recipes in C:

'Minimal' random number generator of Park and Miller with Bayes-Durham
shuffle and added safeguards. Returns a uniform random deviate between 0.0 and
1.0 \(exclusive of the endpoint values\). It has a period of 2^31 - 1 
\(~ 2.1 x 10^9\)"))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object ran1-random-number-generator) &key)
  (with-slots (im ntab) object
    (setf (slot-value object 'am) (/ 1.0d0 im)
          (slot-value object 'ndiv) (+ 1 (/ (- IM 1) NTAB))
          (slot-value object 'rnmx) (- 1.0d0 long-float-negative-epsilon)))
  (initialize-random-number-generator object))

;;; ---------------------------------------------------------------------------

(defmethod initialize-random-number-generator ((rng ran1-random-number-generator))
  (with-slots (ia im am iq ir ntab ndiv rnmx iy iv internal-seed)  
              rng
    (setf internal-seed (float (random-seed rng) 0d0))
    (setf internal-seed (mod (+ (* internal-seed 106) 1283) 6075)
          iy 0.0d0
          iv   (make-array NTAB))
    (do ((j (+ NTAB 7) (1- j)))
        ((< j 0))
      (let ((k (ftruncate internal-seed IQ)))
        (setf internal-seed (- (* IA (- internal-seed (* k IQ))) (* IR k))))
      (if (< internal-seed 0) (incf internal-seed IM))
      (if (< j NTAB) (setf (svref iv j) internal-seed)))
    (setf iy (svref iv 0))))

;;; ---------------------------------------------------------------------------

(defmethod next-element ((rng ran1-random-number-generator))
  (declare (optimize speed))
  (with-slots (ia im am iq ir ndiv rnmx iv iy internal-seed) 
              rng
    (let ((k (ftruncate internal-seed IQ)))
      (setf internal-seed (- (* IA (- internal-seed (* k IQ))) (* IR k))))
    (when (< internal-seed 0d0) (setf internal-seed (+ internal-seed im)))
    (let ((j (floor iy NDIV)))
      (setf iy (svref iv j))
      (setf (svref iv j) internal-seed))
    ;; convert to number between 0.0 and 1.0
    (let ((temp (* AM iy)))
      (if (> temp RNMX) RNMX temp))))


;;; ---------------------------------------------------------------------------
;;; ranq1-random-number-generator
;;; ---------------------------------------------------------------------------

(defclass ranq1-random-number-generator (basic-random-number-generator)
  ((current-value :initform 0d0 :reader current-value))
  (:documentation    "From Numerical Recipes in C:

This is a quick and dirty generator which 'is about as good as any
32-bit linear congruential generator. It is only about 1.7 times faster
than ran1 because of the extra divide we need. It also conses about
1.7 times less. I believe it has the same period as ran1."))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object ranq1-random-number-generator) &key)
  (initialize-random-number-generator object))

;;; ---------------------------------------------------------------------------

(defmethod initialize-random-number-generator ((object ranq1-random-number-generator))
  (setf (slot-value object 'current-value) (random-seed object)))

;;; ---------------------------------------------------------------------------

(defmethod next-element ((rng ranq1-random-number-generator))
  (with-slots (current-value) rng
    (setf current-value
          (logand (+ (* current-value 1664525)
                     1013904223)
                  #xFFFFFFFF))
    (/ current-value #.(float (expt 2 32)))))

#+Test
(let ((g (make-random-number-generator 42 'ranq1-random-number-generator)))
  (delete-file "ccl:ranmt.dat")
  (variates::produce-random-bit-file 
   g "ccl:ranq1.dat" 1000000 25 #\linefeed))

#| Various foolish notions <smile>

;;; ---------------------------------------------------------------------------

(defmethod next-element-of-kind ((rng ranq1-random-number-generator) 
                                 (kind (eql :float)))
  (/ (next-element-of-kind rng :integer) #.(float (expt 2 32))))

;;; ---------------------------------------------------------------------------

(defmethod next-element-of-kind ((rng ranq1-random-number-generator) 
                                 (kind (eql :integer)))
  (with-slots (current-value) rng
    (setf current-value
          (logand (+ (* current-value 1664525)
                     1013904223)
                  #xFFFFFFFF))
    current-value))

(defmethod next-element-of-kind ((rng ran1-random-number-generator) 
                                 (kind (eql :float)))
  (declare (inline next-element))
  (next-element rng))

;;; ---------------------------------------------------------------------------

(defmethod next-element-of-kind ((rng ranq1-random-number-generator) 
                                 (kind (eql :integer)))
  (with-slots (current-value) rng
    (setf current-value
          (logand (+ (* current-value 1664525)
                     1013904223)
                  #xFFFFFFFF))
    current-value))


#+Test
(timeit (:report t)
      (let ((x 0)
            (g (make-random-number-generator 42 'ranq1-random-number-generator)))
        (loop repeat 10000 do
              (setf x (next-element-of-kind g :integer)))
        x))

(timeit (:report t)
      (let ((x 0)
            (g (make-random-number-generator 42 'ran1-random-number-generator)))
        (loop repeat 10000 do
              (setf x (next-element-of-kind g :float)))
        x))

(timeit (:report t)
      (let ((x 0)
            (g (make-random-number-generator 42 'ran1-random-number-generator)))
        (loop repeat 10000 do
              (setf x (next-element g)))
        x))

|#


;;; ---------------------------------------------------------------------------
;;; random-number-generation-mixin
;;; ---------------------------------------------------------------------------

(defclass random-number-generation-mixin ()
  ((random-number-generator 
    :initform nil
    :reader random-number-generator
    :initarg :random-number-generator)
   (random-number-generator-class
    :initform nil
    :reader random-number-generator-class
    :initarg :random-number-generator-class))
  (:default-initargs
    :random-number-generator-class 'ran1-random-number-generator
    :random-seed 42))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object random-number-generation-mixin)
                                       &key random-seed)
  (setf (slot-value object 'random-number-generator)
        (make-instance (random-number-generator-class object) 
          :random-seed random-seed)))

;;; ---------------------------------------------------------------------------

(defmethod next-element ((rng random-number-generation-mixin))
  (next-element (random-number-generator rng)))

;;; ---------------------------------------------------------------------------

(defmethod random-seed ((rng random-number-generation-mixin))
  (random-seed (random-number-generator rng)))

;;; ---------------------------------------------------------------------------

(defmethod (setf random-seed) ((random-seed number) (rng random-number-generation-mixin))
  (setf (random-seed (random-number-generator rng)) random-seed))


;;; ============================================================================
;;; The following functions take generators as arguments.  This makes testing
;;; easier.  Later we see wrapper functions that supply the generators.
;;; ---------------------------------------------------------------------------   

(defun normal-random* (generator mean standard-deviation)
  "Gets a single value sampled from the normal distribution with mean `mean' and
standard devation `standard-deviation.' This uses the algorithm from Numerical
Recipes in C."
  ;; Note that this implementation throws away half of the Gaussian variates.
  ;; This is because we would have to store the other variate with the
  ;; generator, and that seems too cumbersome.
  (do* ((v1 (1- (* 2 (next-element generator))) (1- (* 2 (next-element generator))))
	(v2 (1- (* 2 (next-element generator))) (1- (* 2 (next-element generator))))
	(r (+ (* v1 v1) (* v2 v2)) (+ (* v1 v1) (* v2 v2))))
       ;; if (v1,v2) lie in the unit circle, return
       ;; v1*sqrt(-2.0*log(r)/r) and store v2*sqrt(-2.0*log(r)/r)
       ;; otherwise, try again
       ((< 0 r 1)
	#+ignore
	(setf *standard-normal-deviate-storage*
	      (* v2 (sqrt (* -2.0 (/ (log r) r)))))
	(+ mean (* standard-deviation
		   (* v1 (sqrt (* -2.0 (/ (log r) r)))))))))

;;; ---------------------------------------------------------------------------

(defun exponential-random* (generator rate)
  (/ (- (log (next-element generator))) rate))

;;; ---------------------------------------------------------------------------
;;; Poisson-random
;;; ---------------------------------------------------------------------------

;;; This code is from numerical recipe's but has been de-optimized.
;;; If we find that we're often calling it with the same test-mean, then
;;; we can investigate things like memoization or returning a function or...

(defun poisson-random* (generator test-mean)
  "Returns an integer valued floating point number that is a random
deviate drawn from a Poisson distribution of mean test-mean using the
generator as a source of uniform random deviates. The Poisson distribution
gives the probability of a the number m Poisson random processes ocuring in
a given interval of time."
  (declare (optimize (speed 3) (space 1) (safety 0) (debug 0)) 
           (dynamic-extent test-mean))
  (flet ((do-it (mean)
           (let ((em 0.0)
                 (i 0.0)
                 (y 0.0)
                 (sq 0.0)
                 (alxm 0.0)
                 (g 0.0))
             (declare (type double-float em i y sq alxm g)
                      (dynamic-extent i y sq alxm g mean))
             
             (if (< mean 12.0)
               ;; use direct method
               (progn
                 (setf g (exp (- mean))
                       em -1.0
                       i 1.0)
                 (do ((done-once? nil t))
                     ((and done-once? (<= i g)))
                   (incf em)
                   (setf i (* i (next-element generator)))))
               ;; else, use rejection method
               (progn
                 (setf sq (sqrt (* 2.0 mean))
                       alxm (log mean)
                       g (- (* mean alxm) (gamma-ln (1+ mean))))
                 
                 (do ((done-once-a? nil t))
                     ((and done-once-a? (<= (next-element generator) i)))
                   (do ((done-once-b? nil t))
                       ((and done-once-b? (>= em 0.0)))
                     ;(spy y em g)
                     (setf y (tan (* (next-element generator) 
                                     (coerce pi 'single-float)))
                           em (+ mean (* y sq))))
                   (setf em (float (floor em))
                         i (* 0.9 (1+ (* y y)) 
                              (exp (- (* em alxm) (gamma-ln (1+ em)) g)))))))
             em)))
    (if (typep test-mean 'double-float)
      (do-it test-mean)
      (do-it (coerce test-mean 'double-float)))))

#+Old
;;; This is a straight-forward port of the code in Numerical Recipes in C, 
;;; chapter 7. It's not that fast...
;;;
;; we cache a few things in case the mean doesn't change between calls
(let ((last-mean -1.0)
      (sq 1.0)
      (alxm 1.0)
      (g 1.0))
  (declare (type double-float last-mean sq alxm g))
  (defun poisson-random* (generator mean)
    (declare (optimize (speed 3) (space 1) (safety 0) (debug 0)) 
             (type double-float mean)
             (dynamic-extent mean))
    (let ((em 0.0)
          (i 0.0)
          (y 0.0))
      (declare (type double-float em i y)
               (dynamic-extent i y))
      
      (if (< mean 12.0)
        ;; use direct method
        (progn
          (when (/= mean last-mean)       ; when mean is new, compute exponential
            (setf last-mean mean
                  g (exp (- mean))))
          
          (setf em -1.0
                i 1.0)
          (do ((done-once? nil t))
              ((and done-once? (<= i g)))
            (incf em)
            (setf i (* i (next-element generator)))))
        ;; else, use rejection method
        (progn
          (when (/= mean last-mean)
            (setf last-mean mean
                  sq (sqrt (* 2.0 mean))
                  alxm (log mean)
                  g (- (* mean alxm) (gamma-ln (1+ mean)))))
          
          (do ((done-once-a? nil t))
              ((and done-once-a? (<= (next-element generator) i)))
            (do ((done-once-b? nil t))
                ((and done-once-b? (>= em 0.0)))
              ;(spy y em g)
              (setf y (tan (* (next-element generator) (coerce pi 'single-float)))
                    em (+ mean (* y sq))))
            (setf em (float (floor em))
                  i (* 0.9 (1+ (* y y)) 
                       (exp (- (* em alxm) (gamma-ln (1+ em)) g)))))))
      em)))


;; ---------------------------------------------------------------------------
;;; some utilities
;;; ---------------------------------------------------------------------------

(defvar *random-generator* (make-random-number-generator)
  "This variable takes the place of CL's *random-state*. It can be supplied as
a generator to all the functions in the variates package.")

;;; ---------------------------------------------------------------------------

(deprecated
  "Use shuffle-elements! instance"
  (defun shuffle-list! (list &key (generator *random-generator*) 
                             (times 0 times-supplied?))
    "Destructively rearrange the elements of a list by performing 'times' swaps. If
times is not specified, it will perform 4 times the sequence's length swaps."
    (let ((size (1- (length list))))
      (dotimes (i (if times-supplied? times (* 4 size)))
        (rotatef (elt list (integer-random generator 0 size))
                 (elt list (integer-random generator 0 size)))))
    list))

(defgeneric shuffle-elements! (container &key generator times)
  (:documentation 
    "Destructively rearrange the elements of a container by performing 'times' swaps. If
times is not specified, it will perform 2 times the container's size swaps."))

;;; ---------------------------------------------------------------------------

(defmethod shuffle-elements! ((sequence sequence) &key (generator *random-generator*) 
                              (times 0 times-supplied?))
    (let ((size (1- (length sequence))))
      (dotimes (i (if times-supplied? times (* 2 size)))
        (rotatef (elt sequence (integer-random generator 0 size))
                 (elt sequence (integer-random generator 0 size)))))
    sequence)

;;; ---------------------------------------------------------------------------
;;; cl:random clone
;;; ---------------------------------------------------------------------------

(defun rand (n &optional (seed *random-generator*))
  (etypecase n
    (double-float (uniform-random seed 0.0d0 n))
    ((or integer single-float) (floor (uniform-random seed 0.0d0 (float n 1.0d0))))))

;;; ---------------------------------------------------------------------------
;;; coin flips
;;; ---------------------------------------------------------------------------

(defparameter *probability-of-heads* 0.5d0
  "The default probably used in calls to flip \(and therefore in calls to
binomial and geometric\).")

;;; ---------------------------------------------------------------------------

(defun flip (&optional (pr *probability-of-heads*) (heads t) (tails nil))
  "Flip a pseudo-random coin and return true if it comes up heads. The default
probably of heads for the coin is *probability-of-heads*."
  (if (< (rand 1.0d0) pr) heads tails))

;;; ---------------------------------------------------------------------------
;;; simple random variables
;;; ---------------------------------------------------------------------------

(defun binomial (n p)
  (loop for i from 1 to n 
        summing (flip p 1 0)))

;;; ---------------------------------------------------------------------------

(defun geometric (p)
  (loop for i from 1
        until (flip p)
        finally (return i)))

;;; ---------------------------------------------------------------------------
;;; more randomized algorithm stuff
;;; ---------------------------------------------------------------------------

(defun sample-sequence (seq &key key (pr *probability-of-heads*))
  (unless key 
    (setf key #'identity))
  (flet ((f (x)
           (when (flip pr)
             (funcall key x))))
    (remove-if-not #'f seq)))

;;; ---------------------------------------------------------------------------

(defun select-sample (generator sample-size total-size)
  "Returns a bit vector of size total-size with exactly sample-size
bits set to 1. The one bits are selected uniformly at random. The algorithm
is attributed to Robert Floyd by Jon Bentley in More Programming Pearls." 
  (assert (<= sample-size total-size))
  (let ((set (make-array total-size :element-type 'bit)))
    (labels ((in-set-p (x)
               (= (sbit set x) 1))
             (put-in-set (x)
               (setf (sbit set x) 1))
             (sample-aux (elts total)
               (cond ((zerop elts)
                      nil)
                     (t
                      (sample-aux (1- elts) (1- total))
                      (let ((elt (integer-random generator 0 (1- total))))
                        (if (in-set-p elt)
                          (put-in-set (1- total))
                          (put-in-set elt)))))))
      (sample-aux sample-size total-size)
      set)))

;;; ---------------------------------------------------------------------------

;;?? Gary King 2005-05-10: It appears I wrote this but WtF
;; why shuffle an already random selection?
;; who uses this? anyone? anywhere?
(defun map-unique-indexes (generator sample-size total-size fn &key (shuffle? nil))
  (loop for bit across (let ((bits (select-sample generator sample-size total-size)))
                         (if shuffle? (shuffle-elements! bits :times total-size) bits))
        for index = 0 then (1+ index) 
        unless (zerop bit) do
        (funcall fn index)))


;;; ---------------------------------------------------------------------------
;;; misc
;;; ---------------------------------------------------------------------------

(defun produce-random-bit-file (generator destination bits 
                                          &optional (line-length 25)
                                          (line-feed-char #\linefeed))
  ;; this is handy for using with the NIST test suite
  (with-open-file (s destination
                     :direction :output
                     :if-exists :error
                     :if-does-not-exist :create)
    (princ "  " s)
    (loop for i = 1 then (1+ i)
          repeat bits do
          (princ (if (random-boolean generator 0.5) 1 0) s)
          (when (zerop (mod i line-length)) 
            (princ line-feed-char s)
            (princ "  " s)))))



#| TESTS

(defun graph-random-numbers (count bins generator-function &rest args)
  (let ((generator (make-basic-random-number-generator)))
    (chart:histogram nil (loop repeat count collect (apply generator-function generator args)) :bins bins)))

(graph-random-numbers 1000 20 'uniform-random 0 10)
(graph-random-numbers 1000 20 'exponential-random 12.5)
(graph-random-numbers 1000 20 'normal-random 0 10)
(graph-random-numbers 1000 20 'poisson-random 12.5)

|#

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************