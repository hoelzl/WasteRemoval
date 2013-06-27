(in-package #:waste-prog)

(defvar *environment-type* :small)
(defvar *use-complex-environment* nil)
(defvar *environment* nil)

(defvar *program* #'waste-robot-prog)
(defvar *exploration-strategy* :random)

(defparameter *algorithm-names* (list 
                                 ;; 'hordq
                                 ;; 'gold-standard
                                 'hordq-a-0
                                 'hordq-a-1
                                 'hordq-a-2
                                 'hordq-a-3
                                 ))

(defvar *current-exploration-strategy*)
(defvar *step-number-multiplier* 1)

(defun algorithm-index (algorithm)
  (let ((index (position algorithm *algorithm-names*)))
    (assert index (index) "Algorithm ~A not defined." algorithm)
    index))

(defvar *algorithms* (make-array (list (length *algorithm-names*))
                                 :adjustable t :fill-pointer 0))

(defun algorithm-for (name)
  (aref *algorithms* (algorithm-index name)))

(defun algorithms ()
  *algorithms*)

(defun make-new-environment (&optional (type *environment-type*)
                                       (complexp *use-complex-environment*))
  (ecase type
    ((:small) (if complexp
                  (make-waste-env-1)
                  (make-waste-env-0)))
    ((:medium) (if complexp
                   (make-waste-env-3)
                   (make-waste-env-2)))
    ((:large) (if complexp
                  (make-waste-env-5)
                  (make-waste-env-4)))
    ((:maze :labyrinth) (make-waste-env-6))))

(defun steps-for-environment ()
  (let ((base-size
          (ecase *environment-type*
            ((:small) (if *use-complex-environment*  1000000  500000))
            ((:medium) (if *use-complex-environment* 5000000 2500000))
            ((:large) (if *use-complex-environment* 10000000 5000000))
            ((:maze :labyrinth) (if (eq *exploration-strategy* :random)
                                    3000000 1500000)))))
    (* base-size *step-number-multiplier*)))


(defun initialize-environment (&optional (force t))
  (when (or force (not *environment*))
    (setf *environment* (make-new-environment))))

(defun explore-environment (&optional (recreate nil))
  (initialize-environment (not recreate))
  (env:io-interface *environment*))

(defun initialize-algorithms (&optional (algorithm-names *algorithm-names*))
  (setf *algorithm-names* algorithm-names)
  (setf (fill-pointer *algorithms*) 0)
  (mapc (lambda (alg)
          (when (member (first alg) algorithm-names)
            (vector-push-extend (second alg) *algorithms*)))
        (list
         (list 'smdpq (alisp-smdpq:make-smdpq-alg :hist-out-dir "Temp/"))
         (list 'hordq (make-instance 'ahq:<hordq>))
         (list 'gold-standard (alisp-gold-standard:make-alisp-gold-standard-learning-alg))
         (list 'hordq-a-0 (make-instance 'ahq:<hordq> :features *waste-featurizer-0*))
         (list 'hordq-a-1 (make-instance 'ahq:<hordq> :features *waste-featurizer-1*))
         (list 'hordq-a-2 (make-instance 'ahq:<hordq> :features *waste-featurizer-2*))
         (list 'hordq-a-3 (make-instance 'ahq:<hordq> :features *waste-featurizer-3*))))
  (values))

(defun explore-policies (&optional (show-advice t))
  (initialize-environment nil)
  (set-up-exploration)
  (io-interface *program* *environment*
                (if show-advice
                    (let ((hists (map 'list #'get-q-hist (algorithms))))
                      (mapcan (lambda (hist)
                                (if (and (typep hist 'sequence) (> (length hist) 0))
                                    (list (aref hist (1- (length hist))))
                                    '()))
                              hists))
                    '())))

(defun pick-exploration-strategy (algorithm &optional (strategy *exploration-strategy*))
  (let ((result 
          (ecase strategy
            ((:random)
             'random)
            ((:epsilon)
             (make-instance '<epsilon-policy>
               :q-learning-alg algorithm))
            ((:boltzman)
             (make-instance 'exp-pol:<epsilon-boltzmann-exp-pol>
               :q-learning-alg algorithm
               ;; TODO: Make first parameter depend on number of trials
               :temp-fn (lambda (n) (/ 1000.0 (1+ n)))
               :epsilon-decay-fn (exp-pol:make-linear-epsilon-decay-fn 10000 0.01))))))
    (setf *current-exploration-strategy* result)
    result))

(defun learn-behavior (&key (program *program*)
                            environment-type
                            (use-complex-environment nil use-complex-environment-p)
                            (exploration-strategy *exploration-strategy*)
                            (algorithm-names *algorithm-names*))
  (when environment-type
    (setf *environment-type* environment-type))
  (when use-complex-environment-p
    (setf *use-complex-environment* use-complex-environment))
  (initialize-environment)
  (initialize-algorithms algorithm-names)
  (case exploration-strategy 
    ((:random)
     (format t "~&Learning behavior using random exploration strategy~%")
     (learn program *environment* 'random
            (coerce (algorithms) 'list)
            (steps-for-environment)
            :hist-length 50 :step-print-inc 2500 :episode-print-inc 500))
    (otherwise
     (format t "~&Learning behavior using exploration strategy ~A~%"
             exploration-strategy)
     (map nil
          (lambda (alg)
            (learn program *environment*
                   (pick-exploration-strategy alg exploration-strategy)
                   alg (steps-for-environment)
                   :hist-length 50 :step-print-inc 2500 :episode-print-inc 500))
          (algorithms)))))

(defun evaluation-for (name)
  (evaluate *program* *environment* (get-policy-hist (algorithm-for name))
            :num-steps 50 :num-trials 100))

(defun evaluate-performance (&optional (algorithms *algorithm-names*))
  (format t "~2&Learning curves for ~{~A~^, ~} are:~%" algorithms)
  (pprint (map 'vector #'list 
               (mapcar 'evaluation-for algorithms))))

#+(or)
(defun clean-up ()
  (reset *smdpq*))

(defun print-hash-values (q-getter algorithm)
  (let ((hash (fn-approx:params (q-fn:fn-approx (funcall q-getter algorithm)))))
    (maphash (lambda (k v)
               (format t "~&~A: ~A" k v))
             hash)))

(defun print-qr-values (&optional (algorithm (aref (algorithms) 0)))
  (print-hash-values #'ahq::qr algorithm))
(defun print-qc-values (&optional (algorithm (aref (algorithms) 0)))
  (print-hash-values #'ahq::qc algorithm))
(defun print-qe-values (&optional (algorithm (aref (algorithms) 0)))
  (print-hash-values #'ahq::qe algorithm))
