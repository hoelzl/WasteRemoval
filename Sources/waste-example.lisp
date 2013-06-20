(in-package #:waste-prog)

(defvar *use-large-environment* nil)

(defun make-new-environment (&optional (largep *use-large-environment*))
  (if largep
      (make-test-env-2 :waste-delivery-reward 100.0)
      (make-test-env-1 :waste-delivery-reward 100.0)))

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


(defun algorithm-for (name)
  (ecase name
    ((smdpq) *smdpq*)
    ((hordq) *hordq*)
    ((gold-standard) *gs*)
    ((hordq-a) *hsa*)))

(defparameter *algorithms* (list 
                            ;; 'smdpq
                            ;; 'hordq
                            ;; 'gold-standard
                            'hordq-a))


(defun explore-policies (&optional show-advice)
  (io-interface *prog* *env*
                (if show-advice
                    (let ((hist (get-q-hist *hsa*)))
                      (list (aref hist (1- (length hist)))))
                    '())))

(defun learn-behavior ()
  (learn *prog* *env* 'random
         (mapcar 'algorithm-for *algorithms*) 
         (if *use-large-environment* 500000 100000)
   :hist-length 100 :step-print-inc 100 :episode-print-inc 500))

(defun evaluation-for (name)
  (evaluate *prog* *env* (get-policy-hist (algorithm-for name))
            :num-steps 25 :num-trials 100))

(defun evaluate-performance (&optional (algorithms *algorithms*))
  (format t "~2&Learning curves for ~{~A~^, ~} are:~%" algorithms)
  (pprint (map 'vector #'list 
               (mapcar 'evaluation-for algorithms))))
#+(or)
(defun clean-up ()
  (reset *smdpq*))
