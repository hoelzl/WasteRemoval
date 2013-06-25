(in-package #:waste-utils)

(defclass <epsilon-policy> (<exp-pol>)
  ((algorithm :initarg :q-learning-alg
              :initform (required-initarg :q-learning-alg)
              :reader algorithm
              :type rl-obs:<q-learning-algorithm>)
   (random-choice-fn :initarg :random-choice-fn
                     :initform (lambda (count)
                                 (< (random 1.0) (exp (/ (- count) 500))))
                     :reader random-choice-fn)
   (random-choice-cache :initform (make-hash-table :test 'equalp)
                        :reader random-choice-cache)))

(defparameter *random-choice-counter* 0)

(defmethod initialize-instance :after ((police <epsilon-policy>) &rest initargs &key)
  (declare (ignore initargs))
  (setf *random-choice-counter* 0))

(defmethod get-choice-dist ((policy <epsilon-policy>) state count)
  (if (funcall (random-choice-fn policy) count)
      (let* ((choices (alisp:js-choices state))
             (cached-value (gethash choices (random-choice-cache policy) nil)))
        ;; (incf *random-choice-counter*)
        (or cached-value
            (let ((dist (prob:make-unif-dist-over-set choices)))
              (setf (gethash choices (random-choice-cache policy)) dist)
              ;; (force-format t "~&>>Random choice ~A" *random-choice-counter*)
              dist)))
      (let* ((alg (algorithm policy))
             (base-policy (rl:get-policy alg (rl:knowledge-state alg))))
        (choice-dist base-policy state))))
