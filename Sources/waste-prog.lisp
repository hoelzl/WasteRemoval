(in-package #:waste-prog)

;;; Accessors for the current state.

(def-env-accessor robot-loc ws-robot-loc
    "The current location of the robot.")
(def-env-accessor waste-status ws-waste-status
    "The current status of the waste: either 'at-source, 'on-robot or 'at-dest.")
(def-env-accessor waste-source ws-waste-source
    "The source of the waste for the current state (this will never change during an episode).")
(def-env-accessor waste-target ws-waste-target
    "The target for the waste in the current state (will never change during an episode).")
(def-env-accessor fuel ws-fuel
    "The amount of fuel the robot currently has.")

(defun navigate (loc)
  "navigate LOC 
Navigate to location LOC.  Repeatedly choose among the N, E, S, W, and REFUEL actions until the
robot reaches LOC."
  (until (equal (robot-loc) loc)
    (with-choice navigate-choice (dir '(N E S W refuel))
      (action navigate-move dir))))


(defparameter *refuel-counter* 0)
(defparameter *move-counter* 0)

(defun refuel-and-navigate (loc)
  "refuel-and-navigate LOC 
Navigate to location LOC.  Repeatedly choose among the N, E, S and W actions until the robot
reaches LOC."
  (until (equal (robot-loc) loc)
    (if (<= (fuel) 1.0)
        ;; Always refuel when fuel is low
        (progn
          (incf *refuel-counter*)
          (action refuel-nav 'refuel))
        ;; But still allow refueling when fuel is high
        (progn
          (incf *move-counter*)
          (with-choice navigate-choice (dir '(N E S W refuel))
            (action navigate-move dir))))))

(defun pickup-waste ()
  "pickup-waste
Navigate to the location of the waste and pick it up."
  (call navigate-to-waste (refuel-and-navigate (waste-source)))
  (action pickup-waste 'pickup))

(defun drop-waste ()
  "drop-waste
Navigate to the dropoff location and drop off the waste."
  (call navigate-to-dropoff (navigate (waste-target)))
  (action drop-waste 'drop))

(defun waste-robot-prog ()
  "waste-robot-prog
Repeadedly pick up waste and drop it off."
  (loop
    (choose choose-waste-removal-action
            (call (pickup-waste))
            (call (drop-waste)))))
