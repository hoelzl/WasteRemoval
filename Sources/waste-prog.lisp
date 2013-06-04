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
Navigate to location LOC.  Repeatedly choose among the N, E, S and W actions until the robot
reaches LOC."
  (assert loc (loc) "Location is nil?")
  ;; (force-format t "~&>> navigate to ~A." loc)
  (until (equal (robot-loc) loc)
    (with-choice navigate-choice (dir '(N E S W refuel))
      ;; (force-format t "~&  Loc: ~A; Navigating to: ~A." (robot-loc) dir)
      (action navigate-move dir))))

(defun pickup-waste ()
  "pickup-waste
Navigate to the location of the waste and pick it up."
  ;; (force-format t "~&>> pickup-waste ~A." (waste-source))
  (call navigate-to-waste (navigate (waste-source)))
  ;; (force-format t "~&>>>> Executing pickup action")
  (action pickup-waste 'pickup))

(defun drop-waste ()
  "drop-waste
Navigate to the dropoff location and drop off the waste."
  ;; (force-format t "~&>> drop-waste ~A." (waste-target))
  (call navigate-to-dropoff (navigate (waste-target)))
  ;; (force-format t "~&>>>> Executing pickup action")
  (action drop-waste 'drop))

(defun waste-removal-prog ()
  "waste-removal-prog
Repeadedly pick up waste and drop it off."
  ;; (force-format t "~&>> waste-removal-prog")
  ;; (force-format t "~&  robot-loc: ~A~%  waste-source: ~A~%  waste-target: ~A"
  ;;               (robot-loc) (waste-source) (waste-target))
  (loop
    (choose choose-waste-removal-action
            (call (pickup-waste))
            (call (drop-waste)))))

