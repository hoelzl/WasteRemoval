(in-package #:simple-prog)

(defvar *use-large-environment* nil)

(defun make-new-environment (&optional (largep *use-large-environment*))
  (if largep
      (make-simple-env-2)
      (make-simple-env-1)))

(defparameter *env*
  (make-new-environment))

(defun explore-environment (&optional (recreate nil))
  (when recreate
    (setf *env* (make-new-environment)))
  (env:io-interface *env*))

(defparameter *prog* #'simple-robot-prog)

(defparameter *smdpq* (alisp-smdpq:make-smdpq-alg :hist-out-dir "Temp/"))
(defparameter *hordq* (make-instance 'ahq:<hordq>))
(defparameter *gs* (alisp-gold-standard:make-alisp-gold-standard-learning-alg))
(defparameter *hsa* (make-instance 'ahq:<hordq> :features *simple-featurizer*))


(defun algorithm-for (name)
  (ecase name
    ((smdpq) *smdpq*)
    ((hordq) *hordq*)
    ((gold-standard) *gs*)
    ((hordq-a) *hsa*)))

(defparameter *algorithms* (list 
                            'smdpq
                            'hordq
                            'gold-standard
                            'hordq-a))

(defun algorithms ()
  (mapcar #'algorithm-for *algorithms*))

(defun explore-policies (&optional show-advice)
  (set-up-exploration)
  (io-interface *prog* *env*
                (if show-advice
                    (let ((hists (mapcar #'get-q-hist (algorithms))))
                      (mapcan (lambda (hist)
                                (if (and (typep hist 'sequence) (> (length hist) 0))
                                    (list (aref hist (1- (length hist))))
                                    '()))
                              hists))
                    '())))

(defun learn-behavior ()
  (learn *prog* *env* 'random
         (mapcar 'algorithm-for *algorithms*) 
         (if *use-large-environment* 25000 1000)
   :hist-length 50 :step-print-inc 100 :episode-print-inc 50))

(defun evaluation-for (name)
  (evaluate *prog* *env* (get-policy-hist (algorithm-for name))
            :num-steps 25 :num-trials 25))

(defun evaluate-performance (&optional (algorithms *algorithms*))
  (format t "~2&Learning curves for ~{~A~^, ~} are:~%" algorithms)
  (pprint (map 'vector #'list 
               (mapcar 'evaluation-for algorithms))))
#+(or)
(defun clean-up ()
  (reset *smdpq*))
