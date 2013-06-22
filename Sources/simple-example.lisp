(in-package #:simple-prog)

(defvar *environment-size* :small)
(defvar *environment-size-multiplier* 1)
(defvar *use-complex-environment* nil)


(defparameter *algorithms* (list 
                            ;; 'smdpq
                            ;; 'hordq
                            ;; 'gold-standard
                            ;; 'hordq-a-0
                            ;; 'hordq-a-1
                            'hordq-a-2
                            ;;'hordq-a-3
                            ))


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
            ((:labyrinth) 1000000))))
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

(defvar *random-smdpq*)
(defvar *random-hordq*)
(defvar *random-gs*)
(defvar *random-hsa0*)
(defvar *random-hsa1*)
(defvar *random-hsa2*)
(defvar *random-hsa3*)

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

(defun initialize-random-algorithms ()
  (setf *random-smdpq* (alisp-smdpq:make-smdpq-alg :hist-out-dir "Temp/")
        *random-hordq* (make-instance 'ahq:<hordq>)
        *random-gs* (alisp-gold-standard:make-alisp-gold-standard-learning-alg)
        *random-hsa0* (make-instance 'ahq:<hordq> :features *simple-featurizer-0*)
        *random-hsa1* (make-instance 'ahq:<hordq> :features *simple-featurizer-1*)
        *random-hsa2* (make-instance 'ahq:<hordq> :features *simple-featurizer-2*)
        *random-hsa3* (make-instance 'ahq:<hordq> :features *simple-featurizer-3*)))

(defun algorithm-for (name)
  (ecase name
    ((smdpq) *smdpq*)
    ((hordq) *hordq*)
    ((gold-standard) *gs*)
    ((hordq-a-0) *hsa0*)
    ((hordq-a-1) *hsa1*)
    ((hordq-a-2) *hsa2*)
    ((hordq-a-3) *hsa3*)))

(defun random-algorithm-for (name)
  (ecase name
    ((smdpq) *random-smdpq*)
    ((hordq) *random-hordq*)
    ((gold-standard) *random-gs*)
    ((hordq-a-0) *random-hsa0*)
    ((hordq-a-1) *random-hsa1*)
    ((hordq-a-2) *random-hsa2*)
    ((hordq-a-3) *random-hsa3*)))

(defun algorithms ()
  (mapcar #'algorithm-for *algorithms*))

(defun random-algorithms ()
  (mapcar #'random-algorithm-for *algorithms*))

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

(defvar *exploration-strategy* :random)

(defun pick-exploration-strategy (algorithm &optional (strategy *exploration-strategy*))
  (ecase strategy
    ((:random) 'random)
    ((:greedy) (make-instance 'policy:<greedy-policy>
                 :q-function (rl:get-q-fn algorithm
                                          (rl:knowledge-state algorithm))))))


(defvar *greedy-percentage* 0.3)

(defun learn-behavior ()
  (initialize-environment)
  (initialize-algorithms)
  (when (eq *exploration-strategy* :random-greedy)
    (initialize-random-algorithms))
  (case *exploration-strategy* 
    ((:random)
     (learn *prog* *env* 'random
            (mapcar 'algorithm-for *algorithms*) 
            (steps-for-environment)
            :hist-length 50 :step-print-inc 2500 :episode-print-inc 500))
    ((:random-greedy)
     (mapc (lambda (alg-name)
             (learn *prog* *env*
                    'random
                    (algorithm-for alg-name)
                    (floor (* (steps-for-environment) *greedy-percentage*))
                    :hist-length 25 :step-print-inc 2500 :episode-print-inc 500)
             (learn *prog* *env*
                    (pick-exploration-strategy (algorithm-for alg-name) :greedy)
                    (algorithm-for alg-name)
                    (ceiling (* (steps-for-environment) (- 1.0 *greedy-percentage*)))
                    :hist-length 25 :step-print-inc 2500 :episode-print-inc 500)
             (learn *prog* *env*
                    'random
                    (random-algorithm-for alg-name) (steps-for-environment)
                    :hist-length 50 :step-print-inc 2500 :episode-print-inc 500))
           *algorithms*))
    (otherwise
     (mapc (lambda (alg)
             (learn *prog* *env* (pick-exploration-strategy alg)
                    alg (steps-for-environment)
                    :hist-length 50 :step-print-inc 2500 :episode-print-inc 500))
           (mapcar 'algorithm-for *algorithms*)))))

(defun evaluation-for (name)
  (evaluate *prog* *env* (get-policy-hist (algorithm-for name))
            :num-steps 50 :num-trials 100))

(defun random-evaluation-for (name)
  (evaluate *prog* *env* (get-policy-hist (random-algorithm-for name))
            :num-steps 50 :num-trials 100))

(defun evaluate-performance (&optional (algorithms *algorithms*))
  (format t "~2&Learning curves for ~{~A~^, ~} are:~%" algorithms)
  (pprint (map 'vector #'list 
               (mapcar 'evaluation-for algorithms)))
  (when (eq *exploration-strategy* :random-greedy)
    (format t "~2&Random exploration learning curves for ~{~A~^, ~} are:~%" algorithms)
    (pprint (map 'vector #'list 
                 (mapcar 'random-evaluation-for algorithms)))))
#+(or)
(defun clean-up ()
  (reset *smdpq*))
