(in-package #:common-lisp-user)

(defpackage #:waste-env
  (:use #:common-lisp
        ;; From ALisp
        #:utils
        #:direct-product-set
        #:prob
        #:create-env
        #:grid-world)
  (:export #:<waste-env>
           #:N
	   #:S
	   #:E
	   #:W
	   #:pickup 
	   #:drop
	   #:refuel
           #:init-fuel
           #:fuel-decrease-prob
           #:move-success-prob
           #:wall-collision-cost
           #:cost-of-living
           #:waste-sources
	   #:waste-targets
           #:waste-delivery-reward

           #:waste-state
           #:ws-robot-loc))
