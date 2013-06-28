(in-package #:simple-prog)

(defvar *environment-type* :small)
(defvar *use-complex-environment* nil)
(defvar *environment* nil)

(defvar *program* #'simple-robot-prog)
(defvar *exploration-strategy* :random)

(defparameter *algorithm-names* (list 
                                 ;; 'smdpq
                                 ;; 'hordq
                                 ;; 'gold-standard
                                 ;; 'hordq-a-0
                                 'hordq-a-1
                                 'hordq-a-2
                                 ;; 'hordq-a-3
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
                  (make-simple-env-1)
                  (make-simple-env-0)))
    ((:medium) (if complexp
                   (make-simple-env-3)
                   (make-simple-env-2)))
    ((:large) (if complexp
                  (make-simple-env-5)
                  (make-simple-env-4)))
    ((:maze :labyrinth) (make-simple-env-6))))

(defun steps-for-environment ()
  (let ((base-size
          (ecase *environment-type*
            ((:small) (if *use-complex-environment* 5000 2500))
            ((:medium) (if *use-complex-environment* 100000 25000))
            ((:large) (if *use-complex-environment* 2500000 1000000))
            ((:maze :labyrinth) (if (eq *exploration-strategy* :random)
                                    1000000 500000)))))
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
         (list 'hordq-a-0 (make-instance 'ahq:<hordq> :features *simple-featurizer-0*))
         (list 'hordq-a-1 (make-instance 'ahq:<hordq> :features *simple-featurizer-1*))
         (list 'hordq-a-2 (make-instance 'ahq:<hordq> :features *simple-featurizer-2*))
         (list 'hordq-a-3 (make-instance 'ahq:<hordq> :features *simple-featurizer-3*))))
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
                   :hist-length 100 :step-print-inc 2500 :episode-print-inc 500))
          (algorithms)))))

(defvar *evaluation-steps* 50)
(defvar *evaluation-trials* 25)

(defun evaluation-for (name)
  (evaluate *program* *environment* (get-policy-hist (algorithm-for name))
            :num-steps *evaluation-steps* :num-trials *evaluation-trials*))

(defvar *gnuplot-file-template*
  "set title 'Learning Curve for ~A (Nav, ~A, ~A)'
set title font \",15\"
set xlabel 'iterations'
set xlabel font \",12\"
set ylabel 'reward'
set ylabel font \",12\"
set key off
set autoscale xy
unset grid
set style line 1 lt rgb \"blue\" lw 2 pt 6

set terminal pngcairo
set output '~A'

plot '~A' with linespoints ls 1
")

(defvar *file-exists-action* :error)

(defun evaluate-performance (&key (algorithm-names *algorithm-names*)
                                  (output-directory
                                   (merge-pathnames
                                    (make-pathname :directory '(:relative ".." "Gnuplot"))
                                    (asdf:system-definition-pathname :waste)))
                                  (gnuplot-file-prefix)
                                  (data-file-prefix "data")
                                  (png-file-prefix gnuplot-file-prefix)
                                  (output-to-terminal (not gnuplot-file-prefix)))
  (when output-to-terminal
    (format t "~2&Learning curves for ~{~A~^, ~} are:~%" algorithm-names)
    (pprint (map 'list #'list 
                 (mapcar 'evaluation-for algorithm-names))))
  (when gnuplot-file-prefix
    (ensure-directories-exist output-directory)
    (dolist (alg-name algorithm-names)
      (let* ((file-postfix (concatenate 'string 
                                        "simple-"
                                        (string-downcase (symbol-name alg-name)) "-"
                                        (string-downcase (symbol-name *environment-type*)) "-"
                                        (if *use-complex-environment* "complex" "simple")))
             (data-file (merge-pathnames
                          (make-pathname :name (concatenate 'string
                                                            data-file-prefix 
                                                            file-postfix)
                                         :type "dat")
                          output-directory))
            (gnuplot-file (merge-pathnames
                           (make-pathname :name (concatenate 'string
                                                             gnuplot-file-prefix
                                                             file-postfix)
                                          :type "plt")
                           output-directory))
            (png-file (merge-pathnames
                       (make-pathname :name (concatenate 'string
                                                         png-file-prefix
                                                         file-postfix)
                                      :type "png")
                       output-directory)))
        (format t "~&Writing data to file ~A." (enough-namestring data-file))
        (with-open-file (stream data-file :direction :output
                                            :if-exists *file-exists-action*
                                            :if-does-not-exist :create)
          (map nil 
               (lambda (value)
                 (format stream "~&~A~%" value))
               (let ((*evaluation-steps* (floor (* *evaluation-steps* 2.5))))
                 (evaluation-for alg-name))))
        (format t "~&Writing gnuplot driver file ~A." (enough-namestring gnuplot-file))
        (with-open-file (stream gnuplot-file :direction :output
                                            :if-exists *file-exists-action*
                                            :if-does-not-exist :create)
          (format stream *gnuplot-file-template*
                  alg-name
                  *environment-type*
                  (if *use-complex-environment* "Complex" "Simple")
                  (namestring png-file)
                  (namestring data-file)))))))

#+(or)
(defun clean-up ()
  (reset *smdpq*))
