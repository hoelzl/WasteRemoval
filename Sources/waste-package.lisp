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
   #:*print-graphically*
   #:set-up-exploration
   
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
   #:fuel-amount-per-step
   #:no-fuel-cost
   #:move-success-prob
   #:wall-collision-cost
   #:cost-of-living
   #:waste-sources
   #:waste-targets
   #:waste-delivery-reward

   #:make-waste-env-0
   #:make-waste-env-1
   #:make-waste-env-2
   #:make-waste-env-3
   #:make-waste-env-4
   #:make-waste-env-5
   #:make-waste-env-6

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
        #:waste-utils
        ;; From ALisp
        #:utils
        #:alisp-prog
        #:alisp-features
        #:alisp-user
        #:waste-env)
  (:export
   ;; The Top-Level Program
   #:waste-robot-prog
   
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
