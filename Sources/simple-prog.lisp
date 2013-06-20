(in-package #:simple-prog)

;;; Accessors for the current state.

(def-env-accessor robot-loc simple-robot-loc
    "The current location of the robot.")
(def-env-accessor robot-env simple-env
    "The environment in which the robot operates")

(defun nav (loc)
  "navigate LOC 
Navigate to location LOC.  Repeatedly choose among the N, E, S and W actions until the robot
reaches LOC."
  (until (equal (robot-loc) loc)
    (with-choice navigate-choice (dir '(N E S W))
      (action navigate-move dir))))

(defun simple-robot-prog ()
  "simple-robot-prog
Repeadedly pick up waste and drop it off."
  (loop
    (choose simple-robot-action
            (call (nav (target-loc (robot-env)))))))
