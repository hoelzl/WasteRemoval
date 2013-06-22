(in-package #:simple-env)

;;; A simple grid environment in which a robot should move from a source field to a target
;;; field.  Modeled loosely after the td-taxi world.

(defstruct (simple-state (:conc-name #:simple-))
  start-loc
  robot-loc
  env)

(defmethod clone ((state simple-state))
  (make-simple-state
   :start-loc (simple-start-loc state)
   :robot-loc (simple-robot-loc state)
   :env (simple-env state)))

(defmethod same ((s1 simple-state) (s2 simple-state))
  (and (equal (simple-start-loc s1) (simple-start-loc s2))
       (equal (simple-robot-loc s1) (simple-robot-loc s2))
       (eql (simple-env s1) (simple-env s2))))

(defmethod canonicalize ((state simple-state))
  (list 'start-loc (simple-start-loc state)
        'robot-loc (simple-robot-loc state)))

(defvar *print-graphically* nil)
(defmethod print-object ((state simple-state) stream)
  (if *print-graphically*
      (loop
        with env = (simple-env state)
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
                      ((equal (simple-robot-loc state) (list i j))
                       (format stream "rr"))
                      (t (format stream "  "))))
        finally (format stream "~&Target: ~A"
                        (target-loc env)))
      (call-next-method)))


(defclass <simple-env> (<fully-observable-env> <grid-world>)
  ((move-success-prob :type float
                      :initarg :move-success-prob :initform 0.95
                      :accessor move-success-prob)
   (wall-collision-cost :type float
                        :initarg :wall-collision-cost :initform 0.5
                        :accessor wall-collision-cost)
   (cost-of-living :type float
                   :initarg :cost-of-living :initform 0.1
                   :accessor cost-of-living)
   (start-loc-sampler :type function
                      :initarg :start-loc-sampler
                      :accessor start-loc-sampler)
   (target-loc :type list
               :initarg :target-loc :initform '(0 0)
               :accessor target-loc)
   (final-reward :type float
                 :initarg :final-reward :initform 5.0
                 :accessor final-reward))
  (:default-initargs :legality-test (lambda (val)
                                      (not (eq val 'wall)))))

(defmethod initialize-instance ((env <simple-env>) 
                                &rest initargs &key start-loc-sampler)
  (declare (ignore initargs))
  (call-next-method)
  (unless start-loc-sampler
    (setf (start-loc-sampler env)
          (unif-grid-dist-sampler env))))


(defmethod print-object ((env <simple-env>) stream)
  (assert (not *print-readably*) ()
          "Cannot print instances of <SIMPLE-ENV> readably.")
  (print-unreadable-object (env stream :type t)))

(defvar *available-actions* '(n e s w))

(defmethod avail-actions ((env <simple-env>) state)
  ;; All actions are possible in every state.
  *available-actions*)

(defmethod is-terminal-state ((env <simple-env>) state)
  "is-terminal-state SIMPLE-ENV STATE
A state is terminal if the robot is at the target location."
  (equal (simple-robot-loc state) (target-loc env)))

(defun move-would-hit-wall-p (env state action)
  (destructuring-bind (robot-x robot-y) (simple-robot-loc state)
    (case action
      ((n) (= robot-x 0))
      ((w) (= robot-y 0))
      ((s) (destructuring-bind (dim-x dim-y) (dimensions env)
             (declare (ignore dim-y))
             (= (1+ robot-x) dim-x)))
      ((e) (destructuring-bind (dim-x dim-y) (dimensions env)
             (declare (ignore dim-x))
             (= (1+ robot-y) dim-y))))))

(defun reward (env state action new-state)
  (let* ((at-dest? (equal (simple-robot-loc new-state) (target-loc env)))
         (final-reward (if at-dest?
                           (final-reward env)
                           0.0))
         (hit-wall? (move-would-hit-wall-p env state action))
         (wall-cost (if hit-wall? (wall-collision-cost env) 0.0)))
    (if (is-terminal-state env state)
        0
        (- final-reward
           (cost-of-living env)
           wall-cost))))

(defun move-action-p (action)
  ;; Could also be defined as constantly true in this case.
  (case action
    ((n e s w) t)
    (otherwise nil)))

(defun compute-next-loc (env state action)
  (if (move-action-p action)
      (let* ((loc (simple-robot-loc state))
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
      (simple-robot-loc state)))

(defmethod sample-next ((env <simple-env>) state action)
  (assert (member action (avail-actions env state)) (action)
          "Action ~A is not possible in state ~A." action state)
  (let* ((next-loc (compute-next-loc env state action))
         (new-state (make-simple-state
                     :start-loc (simple-start-loc state)
                     :robot-loc next-loc
                     :env (simple-env state))))
    (values new-state (reward env state action new-state))))

(defun compute-intial-source-loc (env)
  (funcall (start-loc-sampler env)))

(defmethod sample-init ((env <simple-env>))
  (let ((loc (compute-intial-source-loc env)))
    (make-simple-state
     :start-loc loc
     :robot-loc loc
     :env env)))

(defun make-simple-env-0 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(3 4) :initial-element 'road)))
    (apply #'make-instance '<simple-env> :world-map world initargs)))

(defun make-simple-env-1 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(3 4) :initial-element 'road)))
    (setf (aref world 0 2) 'wall
          (aref world 1 2) 'wall)
    (apply #'make-instance '<simple-env> :world-map world initargs)))

(defun make-simple-env-2 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(5 5) :initial-element 'road)))
    #+ (or)
    (setf (aref world 0 2) 'wall
          (aref world 1 2) 'wall)
    (apply #'make-instance '<simple-env> :world-map world initargs)))

(defun make-simple-env-3 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(5 5) :initial-element 'road)))
    (setf (aref world 2 0) 'wall
          (aref world 2 1) 'wall
          (aref world 0 3) 'wall
          (aref world 1 3) 'wall
          (aref world 2 3) 'wall
          (aref world 3 3) 'wall)
    (apply #'make-instance '<simple-env> :world-map world initargs)))

(defun make-simple-env-4 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(10 10) :initial-element 'road)))
    #+ (or)
    (setf (aref world 2 0) 'wall
          (aref world 2 1) 'wall
          (aref world 0 3) 'wall
          (aref world 1 3) 'wall
          (aref world 2 3) 'wall
          (aref world 3 3) 'wall)
    (apply #'make-instance '<simple-env> :world-map world initargs)))

(defun make-simple-env-5 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(10 10) :initial-element 'road)))
    (setf (aref world 2 0) 'wall
          (aref world 2 1) 'wall
          (aref world 0 3) 'wall
          (aref world 1 3) 'wall
          (aref world 2 3) 'wall
          (aref world 3 3) 'wall)
    (apply #'make-instance '<simple-env> :world-map world initargs)))

(defun make-simple-env-6 (&rest initargs &key &allow-other-keys)
  (let ((world (make-array '(10 10) :initial-element 'road))
        (walls '((0 3)
                 (1 3) (1 4) (1 5) (1 6) (1 7) (1 9)
                 (2 0) (2 1) (2 3) (2 5)
                 (3 3) (3 5) (3 6) (3 7) (3 8)
                 (4 1) (4 2) (4 3) (4 5)
                 (5 5) (5 7) (5 8) (5 9)
                 (6 0) (6 1) (6 2) (6 3) (6 5)
                 (7 1) (7 5) (7 6)
                 (8 1) (8 3) (8 5) (8 7)
                 (9 3))))
    (mapc (lambda (wall) (setf (aref world (first wall) (second wall))
                               'wall))
          walls)
    (apply #'make-instance '<simple-env> :world-map world initargs)))

(defun set-up-exploration ()
  (format t "~&Welcome to the simple robot example.

This environment demonstrates a robot that moves around on a rectangular grid, until it reaches
a target area.  X's on the map represent walls, blank spaces are roads.  The robot is
represented by 'r'.  You can move by entering N, E, S, W.  To quit the environment, enter
NIL.  (All input can be in lower or upper case.)")
  (setf *print-graphically* t))

(defmethod io-interface :before ((env <simple-env>))
  (set-up-exploration))
