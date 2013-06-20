(in-package #:common-lisp-user)

(defpackage #:simple-env
  (:use #:common-lisp
        ;; From ALisp
        #:utils
        #:direct-product-set
        #:prob
        #:create-env
        #:grid-world)
  (:export 
   ;; The Environment
   #:<simple-env>
   #:*print-graphically*
   #:set-up-exploration
   
   ;; Actions
   #:N
   #:S
   #:E
   #:W

   ;; Accessors for <waste-env>
   #:move-success-prob
   #:wall-collision-cost
   #:cost-of-living
   #:source-loc
   #:target-loc

   #:make-simple-env-1
   #:make-simple-env-2
   #:make-simple-env-3

   ;; States (Not sure whether this should really be exposed, but we need the functions
   ;; to define state accessors)
   #:simple-robot-loc
   #:simple-env))

(defpackage #:simple-prog
  (:use #:common-lisp
        ;; From ALisp
        #:utils
        #:alisp-prog
        #:alisp-features
        #:alisp-user
        #:simple-env)
  (:export
   ;; The Top-Level Program
   #:simple-robot-prog
   
   ;; Partial Programs
   #:navigate
   
   ;; Actions
   #:N
   #:S
   #:E
   #:W
   
   ;; Environment
   #:<simple-env>
   #:move-success-prob
   #:wall-collision-cost
   #:cost-of-living
   #:source-loc
   #:target-loc
   #:final-reward))
