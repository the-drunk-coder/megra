;; stateful parameter modifier ... (yes, really ...)
;; every one of those needs an "evaluate" function ...
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

;; this one is stateless, not dependent on current value ...
(defclass param-oscillate-between (param-mod-object) nil)

(defmethod evaluate ((o param-oscillate-between))
  (let* ((osc-range (- upper lower))		   
	 (degree-increment (/ 360 cycle o))
	 (degree (mod (* degree-increment (mod step cycle)) 360))
	 (abs-sin (abs (sin (radians degree)))))    
    (+ lower (* abs-sin osc-range))))

(defclass generic-brownian-motion ()
  ((upper-boundary :accessor ubound :initarg :upper-boundary)
   (lower-boundary :accessor lbound :initarg :lower-boundary)
   (step-size :accessor step-size :initarg :step-size)
   (is-bounded :accessor is-bounded :initarg :is-bounded)
   (is-wrapped :accessor is-wrapped :initarg :is-wrapped)))

;; cap or wrap ...
(defmethod cap ((b generic-brownian-motion) value &key)
  (cond ((is-bounded b)
	 (cond ((< value (lbound b)) (lbound b))
	       ((> value (ubound b)) (ubound b))
	       (t value)))
	((is-wrapped b)
	 (cond ((< value (lbound b)) (ubound b))
	       ((> value (ubound b)) (lbound b))
	       (t value)))
	(t value)))

;; this one is stateful ...
(defclass param-brownian-motion (generic-brownian-motion param-mod-object) ())

(defmethod evaluate ((b param-brownian-motion))
  (let* ((new-value (cap b (+ (pmod-current-value b) 
			      (* (nth (random 2) '(-1 1)) (step-size b))))))
    ;; stateful - don't forget to set value ! 
    (setf (pmod-current-value b) new-value)
    ;; return new value
    new-value))


				        


