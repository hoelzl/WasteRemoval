(in-package #:waste-prog)

(def-feature fuel-feature (state action)
  (ws-fuel env-state))

(def-feature robot-dest (state action)
  (stack-var-val 'loc t t))

(def-feature dropping-waste? (state action)
  (stack-contains-frame 'drop-waste))

(def-feature loc (state action)
  (ws-robot-loc env-state))

(def-feature have-waste? (state action)
  (eql (ws-waste-status env-state) 'on-robot))

(def-feature waste-source-feature (state action)
  (ws-waste-source env-state))

(def-feature waste-target-feature (state action)
  (ws-waste-target env-state))

(def-feature waste-dist (state action)
  (grid-world:shortest-path-dist (ws-env env-state)
                                 (ws-waste-source env-state)
                                 (ws-waste-target env-state)))


(defparameter *waste-featurizer*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends fuel-feature loc choice)
    (:qc-depends loc choice robot-dest)
    (:qe-depends have-waste? dropping-waste? waste-dist))
   (navigate-to-waste
    (:qr-depends loc waste-source-feature)
    (:qe-depends waste-dist))
   (navigate-to-dropoff
    (:qr-depends loc waste-target-feature)
    (:qc-depends have-waste?)
    (:qe-depends have-waste? waste-dist))
   (choose-waste-removal-action
    (:qr-depends loc have-waste? waste-source-feature waste-target-feature choice)
    (:qc-depends have-waste? waste-source-feature waste-target-feature choice))))
