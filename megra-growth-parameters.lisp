(defparameter *global-resources* 35.0) ;; just guessing ...
(defparameter *global-regain* 0.2) ;; just guessing ...
(defparameter *global-regain-time* 15) ;; SECONDS ! guess the global regain is the only thing working in seconds 
(defparameter *growth-cost* 1.0) ;; cost to grow one node
(defparameter *autophage-regain* 0.7) ;; resource regain when forced
(defparameter *apoptosis-regain* 0.5) ;; generic regain through planned expiration
(defparameter *default-local-resources* 8) ;; again just guessing
(defparameter *average-node-lifespan* 15) ;; a node survives this many steps on average
(defparameter *node-lifespan-variance* 0.1) 

(defun global-resources (res)
  (setf *global-resources* res))
