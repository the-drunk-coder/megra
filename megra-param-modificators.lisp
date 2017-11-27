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

(defclass generic-oscillate-between ()
  ((upper-boundary :accessor pmod-upper :initarg :upper)
   (lower-boundary :accessor pmod-lower :initarg :lower)   
   (cycle :accessor pmod-cycle :initarg :cycle )
   (type :accessor pmod-osc-type :initarg :type));; not really used yet ... 
  )

;; this one is stateless, not dependent on current value ...
(defclass param-oscillate-between (generic-oscillate-between param-mod-object) nil)

(defmethod evaluate ((o param-oscillate-between))
  (let* ((osc-range (- (pmod-upper o) (pmod-lower o)))		   
	 (degree-increment (/ 360 (pmod-cycle o)))
	 (degree (mod (* degree-increment (mod (pmod-step o) (pmod-cycle o))) 360))
	 (abs-sin (abs (sin (radians degree)))))    
    (+ (pmod-lower o) (* abs-sin osc-range))))

;; constructor
(defun oscil (lower upper &key (cycle 128))
  (make-instance 'param-oscillate-between :lower lower :upper upper :cycle cycle))

(in-package :megra)
(defclass generic-fade ()
  ((from :accessor pmod-from :initarg :from)
   (to :accessor pmod-to :initarg :to)   
   (steps :accessor pmod-steps :initarg :steps)
   (type :accessor pmod-fade-type :initarg :type)
   (current-value :accessor pmod-current-value :initarg :start-value)))

(defclass param-fade (generic-fade param-mod-object) ())

(in-package :megra)

;; so far only sinusoidal fades are supported ... 
(defmethod evaluate ((p param-fade))
  (if (> (pmod-step p) (pmod-steps p))
      (pmod-to p)
      (let* ((osc-range (- (pmod-to p) (pmod-from p)))		   
	     (degree-increment (/ 90 (pmod-steps p)))
	     (degree (* degree-increment (min (pmod-step p) (pmod-steps p)))))    
	(+ (pmod-from p) (* (sin (radians degree)) osc-range)))))

;; fade constructor
(defun fade (from to &key (steps 128))
  (make-instance 'param-fade :from from :to to :steps steps :start-value from))

(defclass generic-brownian-motion ()
  ((upper-boundary :accessor pmod-upper :initarg :upper)
   (lower-boundary :accessor pmod-lower :initarg :lower)
   (step-size :accessor pmod-step-size :initarg :step-size)
   (is-bounded :accessor pmod-is-bounded :initarg :is-bounded)
   (is-wrapped :accessor pmod-is-wrapped :initarg :is-wrapped)
   (current-value :accessor pmod-current-value :initarg :start-value)))

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

(defun brownian (lower upper &key (start 0) (step-size 1) (wrap t) (limit nil))
  (make-instance 'param-brownian-motion
		 :upper upper :lower lower :step-size step-size
		 :is-bounded limit :is-wrapped wrap
		 :start-value start))

(defclass generic-envelope ()
  ((levels :accessor pmod-envelope-levels :initarg :levels :initform nil)
   (steps :accessor pmod-envelope-steps :initarg :steps :initform nil)
   (current-from :accessor pmod-current-from)
   (current-to :accessor pmod-current-to)   
   (current-steps :accessor pmod-current-steps)
   (last-steps :accessor pmod-last-steps :initform 0)
   (done :accessor pmod-done :initform nil)
   (repeat :accessor pmod-repeat :initarg :repeat :initform nil)
   ))

(defclass param-envelope (generic-envelope param-mod-object) ())

(defmethod initialize-instance :after ((p param-envelope) &key)
  (setf (pmod-current-from p) (car (pmod-envelope-levels p)))
  (setf (pmod-current-to p) (cadr (pmod-envelope-levels p)))
  (setf (pmod-current-steps p) (car (pmod-envelope-steps p)))
  ;; drop first values
  (if (pmod-repeat p)
      (progn
	(setf (pmod-envelope-levels p) (append (cdr (pmod-envelope-levels p))
					       (list (car (pmod-envelope-levels p)))))
	(setf (pmod-envelope-steps p) (append (cdr (pmod-envelope-steps p))
					      (list (car (pmod-envelope-steps p))))))
      (progn
	(setf (pmod-envelope-levels p) (cdr (pmod-envelope-levels p)))
	(setf (pmod-envelope-steps p) (cdr (pmod-envelope-steps p))))))
  
(defmethod evaluate :before ((p param-envelope)) 
  (cond ((and (<= (pmod-current-steps p) (- (pmod-step p) (pmod-last-steps p)))
	     (cadr (pmod-envelope-levels p)))  
	 (setf (pmod-last-steps p) (+ (pmod-last-steps p) (pmod-current-steps p)))
	 (setf (pmod-current-from p) (car (pmod-envelope-levels p)))
	 (setf (pmod-current-to p) (cadr (pmod-envelope-levels p)))
	 (setf (pmod-current-steps p) (car (pmod-envelope-steps p)))
	 ;; drop first values
	 (if (pmod-repeat p)
	     (progn
	       (setf (pmod-envelope-levels p) (append (cdr (pmod-envelope-levels p))
						      (list (car (pmod-envelope-levels p)))))
	       (setf (pmod-envelope-steps p) (append (cdr (pmod-envelope-steps p))
						     (list (car (pmod-envelope-steps p))))))
	     (progn
	       (setf (pmod-envelope-levels p) (cdr (pmod-envelope-levels p)))
	       (setf (pmod-envelope-steps p) (cdr (pmod-envelope-steps p))))))
	((and (<= (pmod-current-steps p) (- (pmod-step p) (pmod-last-steps p)))
	      (not (cadr (pmod-envelope-levels p))))
	 (setf (pmod-done p) t))))

(defmethod evaluate ((p param-envelope))
  (if (or (eql (pmod-current-from p) (pmod-current-to p))
	  (pmod-done p))
      (pmod-current-to p)
      (let* ((osc-range (- (pmod-current-to p) (pmod-current-from p)))		   
	     (degree-increment (/ 90 (pmod-current-steps p)))
	     (degree (* degree-increment (min (- (pmod-step p) (pmod-last-steps p))
					      (pmod-current-steps p)))))    
	(+ (pmod-current-from p) (* (sin (radians degree)) osc-range)))))

(defun env (levels steps &key repeat)
  (make-instance 'param-envelope :levels levels :steps steps :repeat repeat))
