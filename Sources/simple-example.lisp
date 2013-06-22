(in-package #:simple-prog)

(defvar *environment-size* :small)
(defvar *environment-size-multiplier* 1)
(defvar *use-complex-environment* nil)


(defparameter *algorithms* (list 
                            ;; 'smdpq
                            ;; 'hordq
                            'gold-standard
                            'hordq-a-0
                            'hordq-a-1
                            'hordq-a-2
                            'hordq-a-3))


(defun make-new-environment (&optional (size *environment-size*)
                                       (complexp *use-complex-environment*))
  (ecase size
    ((:small) (if complexp
                  (make-simple-env-1)
                  (make-simple-env-0)))
    ((:medium) (if complexp
                   (make-simple-env-3)
                   (make-simple-env-2)))
    ((:large) (if complexp
                  (make-simple-env-5)
                  (make-simple-env-4)))
    ((:labyrinth) (make-simple-env-6))))

(defun steps-for-environment ()
  (let ((base-size
          (ecase *environment-size*
            ((:small) (if *use-complex-environment* 5000 2500))
            ((:medium) (if *use-complex-environment* 20000 10000))
            ((:large) (if *use-complex-environment* 500000 250000))
            ((:labyrinth) 500000))))
    (* base-size *environment-size-multiplier*)))

(defvar *env*)

(defun explore-environment (&optional (recreate nil))
  (when (or (not (boundp '*env*)) recreate)
    (setf *env* (make-new-environment)))
  (env:io-interface *env*))

(defparameter *prog* #'simple-robot-prog)

(defvar *smdpq*)
(defvar *hordq*)
(defvar *gs*)
(defvar *hsa0*)
(defvar *hsa1*)
(defvar *hsa2*)
(defvar *hsa3*)

(defun initialize-environment ()
  (setf *env* (make-new-environment)))

(defun initialize-algorithms ()
  (setf *smdpq* (alisp-smdpq:make-smdpq-alg :hist-out-dir "Temp/")
        *hordq* (make-instance 'ahq:<hordq>)
        *gs* (alisp-gold-standard:make-alisp-gold-standard-learning-alg)
        *hsa0* (make-instance 'ahq:<hordq> :features *simple-featurizer-0*)
        *hsa1* (make-instance 'ahq:<hordq> :features *simple-featurizer-1*)
        *hsa2* (make-instance 'ahq:<hordq> :features *simple-featurizer-2*)
        *hsa3* (make-instance 'ahq:<hordq> :features *simple-featurizer-3*)))

(defun algorithm-for (name)
  (ecase name
    ((smdpq) *smdpq*)
    ((hordq) *hordq*)
    ((gold-standard) *gs*)
    ((hordq-a-0) *hsa0*)
    ((hordq-a-1) *hsa1*)
    ((hordq-a-2) *hsa2*)
    ((hordq-a-3) *hsa3*)))
(defun algorithms ()
  (mapcar #'algorithm-for *algorithms*))

(defun explore-policies (&optional (show-advice t))
  (unless (boundp '*env*)
    (initialize-environment))
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
  (initialize-environment)
  (initialize-algorithms)
  (learn *prog* *env* 'random
         (mapcar 'algorithm-for *algorithms*) 
         (steps-for-environment)
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
