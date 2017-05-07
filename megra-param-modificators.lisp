;; stateful parameter modifier ... (yes, really ...)
;; every one of those needs an "evaluate" function ...
(in-package :megra)
(defclass param-mod-object ()
  ((step :accessor pmod-step :initform 0)
   (time :accessor pmod-time :initform 0)
   (current-value :accessor pmod-current-value :initarg :current-value)))

;; before each evaluation, set time ...
(defmethod evaluate :before ((p param-mod-object))
  (setf (pmod-time p) (incudine:now)))

;; after each evaluation, increment step counter
(defmethod evaluate :after ((p param-mod-object))
  (incf (pmod-step p)))

(defclass generic-oscillate-between ()
  ((upper-boundary :accessor pmod-upper :initarg :upper)
   (lower-boundary :accessor pmod-lower :initarg :lower)   
   (cycle :accessor pmod-cycle :initarg :cycle )
   (type :accessor pmod-osc-type :initarg :type)))

;; this one is stateless, not dependent on current value ...
(defclass param-oscillate-between (generic-oscillate-between param-mod-object) nil)

(defmethod evaluate ((o param-oscillate-between))
  (let* ((osc-range (- (pmod-upper o) (pmod-lower o)))		   
	 (degree-increment (/ 360 (pmod-cycle o)))
	 (degree (mod (* degree-increment (mod (pmod-step o) (pmod-cycle o))) 360))
	 (abs-sin (abs (sin (radians degree)))))    
    (+ (pmod-lower o) (* abs-sin osc-range))))

(defclass generic-brownian-motion ()
  ((upper-boundary :accessor pmod-upper :initarg :upper)
   (lower-boundary :accessor pmod-lower :initarg :lower)
   (step-size :accessor pmod-step-size :initarg :step-size)
   (is-bounded :accessor pmod-is-bounded :initarg :is-bounded)
   (is-wrapped :accessor pmod-is-wrapped :initarg :is-wrapped)))

;; cap or wrap ...
(defmethod cap ((b generic-brownian-motion) value &key)
  (cond ((pmod-is-bounded b)
	 (cond ((< value (pmod-upper b)) (pmod-lower b))
	       ((> value (pmod-upper b)) (pmod-upper b))
	       (t value)))
	((pmod-is-wrapped b)
	 (cond ((< value (pmod-lower b)) (pmod-upper b))
	       ((> value (pmod-upper b)) (pmod-lower b))
	       (t value)))
	(t value)))

;; this one is stateful ...
(defclass param-brownian-motion (generic-brownian-motion param-mod-object) ())

(defmethod evaluate ((b param-brownian-motion))
  (let* ((new-value (cap b (+ (pmod-current-value b) 
			      (* (nth (random 2) '(-1 1)) (pmod-step-size b))))))
    ;; stateful - don't forget to set value ! 
    (setf (pmod-current-value b) new-value)
    ;; return new value
    new-value))


				        


