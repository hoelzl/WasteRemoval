(in-package #:waste-prog)

(defvar *use-large-environment* nil)
(defparameter *env*
  (if *use-large-environment*
      (make-test-env-2)
      (make-test-env-1)))

(defun explore-environment ()
  (env:io-interface *env*))

(defparameter *prog* #'waste-removal-prog)

(defparameter *smdpq* (alisp-smdpq:make-smdpq-alg
                       :hist-out-dir "Temp/"))
(defparameter *hordq* (make-instance 'ahq:<hordq>))
(defparameter *gs* (alisp-gold-standard:make-alisp-gold-standard-learning-alg))
; (defparameter *hsa* (make-instance 'ahq:<hordq> :features *featurizer*))


(defparameter *algorithms* (list *smdpq* *hordq* *gs*))

(defun learn-behavior ()
  (learn *prog* *env* 'random *algorithms* 10000
   :hist-length 25))

(defun evaluate-performance ()
  (let ((sq-rews (evaluate *prog* *env* (get-policy-hist *smdpq*)
                           :num-steps 25 :num-trials 5))
        (hq-rews
          (evaluate *prog* *env* (get-policy-hist *hordq*)
                    :num-steps 25 :num-trials 5))
        (gs-rews
          (evaluate *prog* *env* (get-policy-hist *gs*)
                    :num-steps 25 :num-trials 5))
        #+(or)
        (hqs-rews
          (evaluate *prog* *env* (get-policy-hist *hsa*)
                    :num-steps 25 :num-trials 5)))
    (format t "~%~%Learning curves for SMDPQ, HORDQ, HORDQ-SA, and GS are:~%")
    (pprint (map 'vector #'list sq-rews hq-rews #+(or) hqs-rews gs-rews))))

(defun clean-up ()
  (reset *smdpq*))
