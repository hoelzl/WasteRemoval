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
