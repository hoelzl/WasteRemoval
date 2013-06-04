(in-package #:waste-prog)

(defvar *use-large-environment* nil)

(defun make-new-environment (&optional (largep *use-large-environment*))
  (if largep
      (make-test-env-2)
      (make-test-env-1)))

(defparameter *env*
  (make-new-environment))

(defun explore-environment (&optional (recreate nil))
  (when recreate
    (setf *env* (make-new-environment)))
  (env:io-interface *env*))

(defparameter *prog* #'waste-removal-prog)

(defparameter *smdpq* (alisp-smdpq:make-smdpq-alg :hist-out-dir "Temp/"))
(defparameter *hordq* (make-instance 'ahq:<hordq>))
(defparameter *gs* (alisp-gold-standard:make-alisp-gold-standard-learning-alg))
(defparameter *hsa* (make-instance 'ahq:<hordq> :features *waste-featurizer*))

(defun explore-policies (&optional show-advice)
  (io-interface *prog* *env*
                (if show-advice
                    (let ((hist (get-q-hist *hsa*)))
                      (list (aref hist (1- (length hist)))))
                    '())))

(defparameter *algorithms* (list 
                            ;; *smdpq*
                            ;; *hordq*
                            ;; *gs*
                            *hsa*))

(defun learn-behavior ()
  (learn *prog* *env* 'random *algorithms* 
         (if *use-large-environment* (* 100 50000) 15000)
   :hist-length 100 :step-print-inc 100 :episode-print-inc 500))

(defun evaluate-performance ()
  (let (#+(or)
        (sq-rews (evaluate *prog* *env* (get-policy-hist *smdpq*)
                           :num-steps 25 :num-trials 5))
        #+(or)
        (hq-rews
          (evaluate *prog* *env* (get-policy-hist *hordq*)
                    :num-steps 25 :num-trials 5))
        #+(or)
        (gs-rews
          (evaluate *prog* *env* (get-policy-hist *gs*)
                    :num-steps 25 :num-trials 5))
        #+(and)
        (hqs-rews
          (evaluate *prog* *env* (get-policy-hist *hsa*)
                    :num-steps 25 :num-trials 100)))
    (format t "~%~%Learning curves are:~%")
    (pprint (map 'vector #'list 
                 ;; sq-rews 
                 ;; hq-rews 
                 ;; gs-rews 
                 hqs-rews))))
#+(or)
(defun clean-up ()
  (reset *smdpq*))
