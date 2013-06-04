(in-package #:waste-env)

;;; A waste disposal environment.  Modeled loosely after the td-taxi world.

(defstruct (waste-state (:conc-name #:ws-))
  robot-loc
  (waste-status 'at-source) ; on-robot at-source at-dest
  waste-source
  waste-target
  fuel
  env)

(defmethod clone ((state waste-state))
  (make-waste-state
   :robot-loc (ws-robot-loc state)
   :waste-status (ws-waste-status state)
   :waste-source (ws-waste-source state)
   :waste-target (ws-waste-target state)
   :fuel (ws-fuel state)
   :env (ws-env state)))

(defmethod same ((s1 waste-state) (s2 waste-state))
  (and (equal (ws-robot-loc s1) (ws-robot-loc s2))
       (eql (ws-waste-status s1) (ws-waste-status s2))
       (equal (ws-waste-source s1) (ws-waste-source s2))
       (equal (ws-waste-target s1) (ws-waste-target s2))
       (= (ws-fuel s1) (ws-fuel s2))
       (eql (ws-env s1) (ws-env s2))))

(defmethod canonicalize ((state waste-state))
  (list 'robot-loc (ws-robot-loc state)
        'waste-status (ws-waste-status state)
        'waste-source (ws-waste-source state)
        'waste-target (ws-waste-target state)
        'fuel (ws-fuel state)))

(defvar *print-graphically* nil)
(defmethod print-object ((state waste-state) stream)
  (if *print-graphically*
      (loop
        with env = (ws-env state)
        with d = (dimensions env)
        for i from -1 to (first d)
        do (terpri stream)
        do (loop
             for j from -1 to (second d)
             do (cond ((or (= i -1) (= i (first d)))
                       (format stream "XX"))
                      ((or (= j -1) (= j (second d)))
                       (format stream "XX"))
                      ((eq (loc-value env (list i j)) 'wall) 
                       (format stream "XX"))
                      ((equal (ws-robot-loc state) (list i j))
                       (if (eq (ws-waste-status state) 'on-robot)
                           (format stream "RR")
                           (format stream "rr")))
                      ((equal (ws-waste-source state) (list i j))
                       (if (eq (ws-waste-status state) 'at-source)
                           (format stream "WW")
                           (format stream "ww")))
                      (t (format stream "  "))))
        finally (format stream "~&Waste Targets: ~A  Fuel: ~A"
                        (waste-targets env)  (ws-fuel state)))
      (call-next-method)))


(defclass <waste-env> (<fully-observable-env> <grid-world>)
  ((init-fuel :type fixnum
              :initarg :init-fuel :initform 3.0
              :accessor init-fuel)
   (fuel-decrease-prob :type float
                       :initarg :fuel-decrease-prob :initform 0.8
                       :accessor fuel-decrease-prob)
   (fuel-amount-per-step :type float
                         :initarg :fuel-amount-per-step :initform 1.0
                         :accessor fuel-amount-per-step)
   (no-fuel-cost :type float
                 :initarg :no-fuel-cost :initform 1.0
                 :accessor no-fuel-cost)
   (refuel-success-prob :type float
                        :initarg :refuel-success-prob :initform 0.95
                        :accessor refuel-success-prob)
   (move-success-prob :type float
                      :initarg :move-success-prob :initform 0.95
                      :accessor move-success-prob)
   (wall-collision-cost :type float
                        :initarg :wall-collision-cost :initform 0.5
                        :accessor wall-collision-cost)
   (cost-of-living :type float
                   :initarg :cost-of-living :initform 0.1
                   :accessor cost-of-living)
   (waste-sources :type list
                  :initarg :waste-sources
                  :accessor waste-sources)
   (waste-targets :type list
                  :initarg :waste-targets :initform '((0 0))
                  :accessor waste-targets)
   (waste-delivery-reward :type float
                          :initarg :waste-delivery-reward :initform 5.0
                          :accessor waste-delivery-reward))
  (:default-initargs :legality-test (lambda (val)
                                      (not (eq val 'wall)))))

(defmethod initialize-instance ((env <waste-env>)
                                &rest initargs &key waste-sources world-map)
  (declare (ignore initargs))
  (unless waste-sources
    (let ((sources (make-instance '<prod-set>
                     :sets (array-dimensions world-map)
                     :alist-keys '(0 1))))
      (setf (waste-sources env) sources)))
  (call-next-method))

(defmethod print-object ((env <waste-env>) stream)
  (if *print-readably*
      (progn
        (assert nil () "Who printed me?")
        )
      (print-unreadable-object (env stream :type t))))

(defvar *available-actions* '(n e s w pickup drop refuel))

(defmethod avail-actions ((env <waste-env>) state)
  ;; All actions are possible in every state.
  *available-actions*)

(defmethod is-terminal-state ((env <waste-env>) state)
  "is-terminal-state WASTE-ENV STATE
A state is terminal if we have unloaded the waste at one of the waste targets."
  (or (eq (ws-waste-status state) 'at-dest)
      (zerop (ws-fuel state))))

(defun reward (env state action new-state)
  (declare (ignore action))
  (let* ((waste-at-dest? (eq (ws-waste-status new-state) 'at-dest))
         (waste-reward (if waste-at-dest?
                           (waste-delivery-reward env)
                           0.0))
         (hit-wall? (eq (ws-robot-loc state) (ws-robot-loc new-state)))
         (wall-cost (if hit-wall? (wall-collision-cost env) 0.0))
         (no-fuel? (zerop (ws-fuel state)))
         (no-fuel-cost (if no-fuel? (no-fuel-cost env) 0.0)))
    (if (is-terminal-state env state)
        0
        (- waste-reward
           (cost-of-living env)
           wall-cost
           no-fuel-cost))))

(defun move-action-p (action)
  (case action
    ((n e s w) t)
    (otherwise nil)))

(defun compute-next-loc (env state action)
  (if (and (move-action-p action)
           (> (ws-fuel state) 0))
      (let* ((loc (ws-robot-loc state))
             (succ-prob (move-success-prob env))
             (slip-prob (/ (- 1.0 succ-prob) 2.0))
             (forward-loc (result loc action))
             (forward-prob (if (is-legal-loc env forward-loc) succ-prob 0.0))
             (left-loc (result loc (rot-counterclockwise action)))
             (left-prob (if (is-legal-loc env left-loc) slip-prob 0.0))
             (right-loc (result loc (rot-clockwise action)))
             (right-prob (if (is-legal-loc env right-loc) slip-prob 0.0))
             (stay-prob (- 1.0 (+ forward-prob left-prob right-prob))))
        (sample-multinomial (list forward-loc right-loc left-loc loc)
                            forward-prob right-prob left-prob stay-prob))
      (ws-robot-loc state)))

(defun compute-waste-status (env state action)
  (let ((waste-status (ws-waste-status state)))
    (case action
      ((pickup)
       (if (equal (ws-robot-loc state) (ws-waste-source state))
           'on-robot
           waste-status))
      ((drop)
       (if (and (eq (ws-waste-status state) 'on-robot)
                (member (ws-robot-loc state) (waste-targets env) :test 'equal))
           'at-dest
           waste-status))
      (otherwise
       waste-status))))

(defun compute-fuel (env state action)
  (cond ((move-action-p action)
         (let ((fuel (ws-fuel state))
               (fuel-prob (fuel-decrease-prob env)))
           (sample-multinomial (list (max 0.0 (- fuel (fuel-amount-per-step env))) fuel)
                               fuel-prob (- 1.0 fuel-prob))))
        ((eq action 'refuel)
         (let ((refuel-prob (refuel-success-prob env)))
           (sample-multinomial (list (ws-fuel state) (init-fuel env))
                               (- 1.0 refuel-prob) refuel-prob)))
        (t (ws-fuel state))))

(defmethod sample-next ((env <waste-env>) state action)
  (assert (member action (avail-actions env state)) (action)
          "Action ~A is not possible in state ~A." action state)
  (let* ((next-loc (compute-next-loc env state action))
         (waste-status (compute-waste-status env state action))
         (fuel (compute-fuel env state action))
         (new-state (make-waste-state
                     :robot-loc next-loc
                     :waste-status waste-status
                     :waste-source (ws-waste-source state)
                     :waste-target (ws-waste-target state)
                     :fuel fuel
                     :env (ws-env state))))
    (values new-state (reward env state action new-state))))

(defvar *max-initial-waste-source-tries* 100)

(defun compute-intial-waste-source (env)
  (loop for i from 0 to *max-initial-waste-source-tries*
        do (let ((loc (mapcar #'cdr (sample-uniformly (waste-sources env)))))
             (when (is-legal-loc env loc)
               (return-from compute-intial-waste-source loc))))
  (error "Could not find an initial position in ~A." env))

(defmethod sample-init ((env <waste-env>))
  (make-waste-state
   :robot-loc (funcall (unif-grid-dist-sampler env))
   :waste-status 'at-source
   :waste-source (compute-intial-waste-source env)
   :waste-target (first (waste-targets env))
   :fuel (init-fuel env)
   :env env))

(defun make-test-env-1 ()
  (let ((world (make-array '(3 4) :initial-element 'road)))
    (make-instance '<waste-env> :world-map world)))

(defun make-test-env-2 ()
  (let ((world (make-array '(5 5) :initial-element 'road)))
    (setf (aref world 3 3) 'wall
          (aref world 4 3) 'wall)
    (make-instance '<waste-env> :world-map world)))

(defun make-test-env-3 ()
  (let ((world (make-array '(5 5) :initial-element 'road)))
    (setf (aref world 2 1) 'wall
          (aref world 2 2) 'wall
          (aref world 2 3) 'wall
          (aref world 3 3) 'wall
          (aref world 4 3) 'wall)
    (make-instance '<waste-env> :world-map world)))

#||
(defparameter *test-env*
  (make-test-env-2))
(defparameter *s1* (sample-next *test-env* (get-state *test-env*) 's))
(defparameter *s2* (sample-next *test-env* *s1* 's))
||#

(defmethod io-interface :before ((env <waste-env>))
  (format t "~&Welcome to the robotic waste collection example.

This environment demonstrates a robot that moves around on a rectangular grid, picks up waste
and delivers it to a drop-off zone.  X's on the map represent walls, blank spaces are roads.
The robot is represented by 'r' as long as it does not carry waste, by 'R' as soon as it has
picked up waste.  You can move by entering N, E, S, W; if the robot is on the same grid field as
the waste you can pick it up by entering PICKUP, if you are in a drop-off zone you can drop the
waste and thereby end the episode (and collect the reward) by entering DROP.  To quit the
environment, enter NIL.  (All input can be in lower or upper case.)")
  (setf *print-graphically* t))
