(in-package #:common-lisp-user)

(defpackage #:waste-env
  (:use #:common-lisp
        ;; From ALisp
        #:utils
        #:direct-product-set
        #:prob
        #:create-env
        #:grid-world)
  (:export 
   ;; The Environment
   #:<waste-env>
   
   ;; Actions
   #:N
   #:S
   #:E
   #:W
   #:pickup 
   #:drop
   #:refuel

   ;; Accessors for <waste-env>
   #:init-fuel
   #:fuel-decrease-prob
   #:move-success-prob
   #:wall-collision-cost
   #:cost-of-living
   #:waste-sources
   #:waste-targets
   #:waste-delivery-reward

   #:make-test-env-1
   #:make-test-env-2

   ;; States (Not sure whether this should really be exposed, but we need the functions
   ;; to define state accessors)
   #:waste-state
   #:ws-robot-loc
   #:ws-waste-status
   #:ws-waste-source
   #:ws-waste-target
   #:ws-fuel
   #:ws-env

   ;; The waste status 
   #:on-robot
   #:at-source
   #:at-dest))

(defpackage #:waste-prog
  (:use #:common-lisp
        ;; From ALisp
        #:utils
        #:alisp-prog
        #:alisp-user
        #:waste-env)
  (:export
   ;; The Top-Level Program
   #:waste-removal-prog
   
   ;; Partial Programs
   #:navigate
   #:pickup-waste
   #:drop-wate
   
   ;; Actions
   #:N
   #:S
   #:E
   #:W
   #:pickup 
   #:drop
   #:refuel
   
   ;; Environment
   #:<waste-env>
   #:init-fuel
   #:fuel-decrease-prob
   #:move-success-prob
   #:wall-collision-cost
   #:cost-of-living
   #:waste-sources
   #:waste-targets
   #:waste-delivery-reward))
