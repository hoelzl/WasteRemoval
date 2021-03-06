(in-package #:simple-prog)

(def-feature loc (state action)
  (simple-robot-loc env-state))

(defvar *quantize-target-dist* nil)

(def-feature target-dist (state action)
  (let* ((env (simple-env env-state))
         (dist (grid-world:shortest-path-dist env
                                              (robot-loc)
                                              (target-loc env))))
    #+ (or)
    (force-format t "~&>>> target dist: ~A, ~A = ~A~%"
                  (robot-loc) (target-loc env) dist)
    (if *quantize-target-dist*
        (cond ((< dist 3) :near)
              ((< dist 6) :medium)
              (t :far))
        dist)))

(defun is-valid-direction-p (action target-x target-y robot-x robot-y)
  (let* ((x-action (cond ((< target-x robot-x) 'w)
                         ((> target-x robot-x) 'e)
                        (t '())))
         (y-action (cond ((< target-y robot-y) 'n)
                         ((> target-y robot-y) 's)
                        (t '())))
         (res (or (eql action x-action) (eql action y-action))))
    #+ (or)
    (force-format t "~&>>> is-valid-direction-p ~A (~A, ~A) (~A, ~A): ~A"
                  action robot-x robot-y target-x target-y res)
    res))

(def-feature target-direction-valid-p (state action)
  (let ((env (simple-env env-state)))
    (destructuring-bind (target-x target-y) (target-loc env)
      (destructuring-bind (robot-x robot-y) (simple-robot-loc env-state)
        (is-valid-direction-p
         action target-x target-y robot-x robot-y)))))

(def-feature shortest-path-distance (state action)
  (let* ((robot-loc (simple-robot-loc env-state))
         (target-loc (stack-var-val 'loc t t)))
    (grid-world:shortest-path-dist (simple-env env-state)
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
  (let* ((robot-loc (simple-robot-loc env-state))
         (target-loc (target-loc (simple-env env-state)))
         (target-path (grid-world:shortest-path (simple-env env-state)
                                                robot-loc
                                                target-loc)))
    (assert target-loc (target-loc) "No target location?")
    (cond ((equal robot-loc target-loc) 'rest)
          (t
           (assert (>= (length target-path) 2))
           (direction-from-to robot-loc (second target-path))))))

(defparameter *simple-featurizer-0*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends choice)
    (:qc-depends)
    (:qe-depends))
   (simple-robot-action
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (nav
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))))

(defparameter *simple-featurizer-1*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends target-direction-valid-p choice)
    (:qc-depends)
    (:qe-depends))
   (simple-robot-action
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (nav
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))))

(defparameter *simple-featurizer-2*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc target-direction-valid-p choice)
    (:qc-depends loc choice)
    (:qe-depends))
   (simple-robot-action
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (nav
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))))

(defparameter *simple-featurizer-3*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends loc target-direction-valid-p choice)
    (:qc-depends loc choice)
    (:qe-depends))
   (simple-robot-action
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))
   (nav
    (:qr-depends)
    (:qc-depends)
    (:qe-depends))))

#+ (or)
(defparameter *simple-featurizer-4*
  (make-3partq-featurizer
   ()
   (navigate-choice
    (:qr-depends choice target-direction-valid-p loc)
    (:qc-depends choice target-direction-valid-p loc)
    (:qe-depends choice target-direction-valid-p loc))
   (simple-robot-action
    (:qr-depends choice)
    (:qc-depends choice)
    (:qe-depends choice))))
