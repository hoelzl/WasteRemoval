(in-package #:simple-prog)

(def-feature loc (state action)
  (simple-robot-loc env-state))

#+ (or)
(def-feature target-dist (state action)
  (let ((env (simple-env env-state)))
    (grid-world:shortest-path-dist env
                                   (robot-loc env-state)
                                   (target-loc env-state))))

(defun compute-valid-directions (action target-x target-y robot-x robot-y)
  (let ((x-action (cond ((< target-x robot-x) '(w))
                        ((> target-x robot-x) '(e))
                        (t '())))
        (y-action (cond ((< target-y robot-y) '(n))
                        ((> target-y robot-y) '(s))
                              (t '()))))
    (member action (append x-action y-action))))

(def-feature target-direction (state action)
  (let ((env (simple-env env-state)))
    (destructuring-bind (target-x target-y) (target-loc env)
      (destructuring-bind (robot-x robot-y) (simple-robot-loc env-state)
        (indicator (compute-valid-directions
                    action target-x target-y robot-x robot-y))))))

(defparameter *simple-featurizer*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc target-direction choice)
    (:qc-depends loc target-direction choice)
    (:qe-depends loc target-direction choice))
   (simple-robot-action
    (:qr-depends choice)
    (:qc-depends choice)
    (:qe-depends choice))))
