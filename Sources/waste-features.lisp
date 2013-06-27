(in-package #:waste-prog)

(def-feature fuel-feature (state action)
  (ws-fuel env-state))

(def-feature fuel-critical-p (state action)
  (let ((env (ws-env env-state)))
    ;; We're bingo fuel...
    (if (<= (ws-fuel env-state) (fuel-amount-per-step env))
        t
        nil)))

(def-feature loc (state action)
  (ws-robot-loc env-state))

(def-feature have-waste? (state action)
  (eql (ws-waste-status env-state) :on-robot))

(def-feature waste-source-feature (state action)
  (ws-waste-source env-state))

(def-feature waste-target-feature (state action)
  (ws-waste-target env-state))

(def-feature robot-dest (state action)
  (stack-var-val 'loc t t))

(def-feature navigating-to-waste (state action)
  (stack-contains-frame 'pickup-waste))

;;; Superfluous, since it currently is always the negation of NAVIGATING-TO-WASTE
(def-feature navigating-to-dropoff (state action)
  (stack-contains-frame 'drop-waste))

(def-feature waste-dist (state action)
  (grid-world:shortest-path-dist (ws-env env-state)
                                 (ws-waste-source env-state)
                                 (ws-waste-target env-state)))

(def-feature act-dist (state action)
  (grid-world:shortest-path-dist (ws-env env-state)
                                 (ws-robot-loc env-state)
                                 (ws-waste-target env-state)))

(def-feature shortest-path-distance (state action)
  (let* ((robot-loc (ws-robot-loc env-state))
         (target-loc (stack-var-val 'loc t t)))
    (grid-world:shortest-path-dist (ws-env env-state)
                                   robot-loc
                                   target-loc)))

(defun direction-from-to (from to)
  (destructuring-bind (from-x from-y) from
    (destructuring-bind (to-x to-y) to
      (cond ((< from-x to-x) 's)
            ((> from-x to-x) 'n)
            ((< from-y to-y) 'e)
            ((> from-y to-y) 'w)
            (t 'rest)))))

(def-feature shortest-path-direction (state action)
  (let* ((robot-loc (ws-robot-loc env-state))
         (target-loc (ws-waste-target env-state))
         (target-path (grid-world:shortest-path (ws-env env-state)
                                               robot-loc
                                               target-loc)))
    (assert target-loc (target-loc) "No target location?")
    (cond ((equal robot-loc target-loc) 'rest)
          (t
           (assert (>= (length target-path) 2))
           (direction-from-to robot-loc (second target-path))))))


(defparameter *waste-featurizer-0*
  (make-3partq-featurizer
   ()
   ((navigate-choice
     (:qr-depends loc choice navigating-to-waste)
     (:qc-depends)
     (:qe-depends)))
   (navigate-to-waste
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (navigate-to-dropoff
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (choose-waste-removal-action
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))))

(defparameter *waste-featurizer-1*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc choice navigating-to-waste)
    (:qc-depends loc choice navigating-to-waste
                 shortest-path-direction)
    (:qe-depends loc choice))
   (navigate-to-waste
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (navigate-to-dropoff
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (choose-waste-removal-action
    (:qr-depends)
    (:qc-depends)
    (:qe-dependes))))

(defparameter *waste-featurizer-2*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc choice navigating-to-waste)
    (:qc-depends choice navigating-to-waste
                 shortest-path-distance shortest-path-direction)
    (:qe-depends choice navigating-to-waste
                 shortest-path-distance shortest-path-direction))
   (navigate-to-waste
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (navigate-to-dropoff
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (choose-waste-removal-action
    (:qr-depends)
    (:qc-depends)
    (:qe-dependes))))

(defparameter *waste-featurizer-3*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc choice navigating-to-waste)
    (:qc-depends choice navigating-to-waste
                 shortest-path-distance shortest-path-direction)
    (:qe-depends choice shortest-path-direction))
   (navigate-to-waste
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (navigate-to-dropoff
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (choose-waste-removal-action
    (:qr-depends)
    (:qc-depends)
    (:qe-dependes))))
