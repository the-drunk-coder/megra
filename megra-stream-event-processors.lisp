;; a collection of processors modifying the event stream
(in-package :megra)

;; with the advent of param-mod-objects, some of these might be deemed deprecated,
;; but left for legacy reasons ...
(defclass stream-event-processor (event-processor)
  ((property :accessor modified-property :initarg :mod-prop)
   (last-values-by-source :accessor lastval)
   (affect-transition :accessor affect-transition :initarg :affect-transition)
   (track-state :accessor track-state :initarg :track-state :initform t)
   (event-filter :accessor event-filter :initarg :event-filter)
   (step :accessor pmod-step :initform 0)))

(defmethod initialize-instance :after ((m stream-event-processor) &key)
  (setf (lastval m) (make-hash-table :test 'eql)))

(defmethod pull-transition ((e stream-event-processor) &key)
  (if (successor e)
      (progn
	(current-transition e)        
	(if (affect-transition e)
	    (apply-self e (pull-transition (successor e)))
	    (pull-transition (successor e))))
      (current-transition e)))

(defmethod apply-self :before ((m stream-event-processor) events &key)
  ;; state tracking 
  (mapc #'(lambda (event)
	    (if (event-has-slot-by-name event (modified-property m))
		(unless (gethash (event-source event) (lastval m))  
		  (setf (gethash (event-source event) (lastval m))
			(slot-value event (modified-property m))))
		event))
	events))

(Defmethod apply-self :after ((m stream-event-processor) events &key)
  (setf (pmod-step m) (1+ (pmod-step m))))
	   
(defmethod get-current-value ((m stream-event-processor) (e event) &key)
  (if (track-state m)
      (gethash (event-source e) (lastval m))
      (slot-value e (modified-property m))))

(defmethod filter-events ((m stream-event-processor) events
			  &key (check-mod-prop t))
  (labels ((current-filter-p (event)
	     (if check-mod-prop
		 (and (event-has-slot-by-name event (modified-property m))
				 (funcall (event-filter m) event))
		 (funcall (event-filter m) event))))
    (remove-if-not #'current-filter-p events)))
;; switch to preserve/not preserve state ?


;; oscillate a parameter between different values in a stream
(defclass stream-oscillate-between (generic-oscillate-between stream-event-processor) ())

;; helper ...
(defun radians (numberOfDegrees) 
  (* pi (/ numberOfDegrees 180.0)))

(defmethod apply-self ((o stream-oscillate-between) events &key)
  (mapc #'(lambda (event)
	    (let* ((osc-range (- (pmod-upper o) (pmod-lower o)))		   
		   (degree-increment (/ 360 (pmod-cycle o)))
		   (degree (mod (* degree-increment (mod (pmod-step o) (pmod-cycle o))) 360))
		   (abs-sin (abs (sin (radians degree))))		   
		   (new-value (+ (pmod-lower o) (* abs-sin osc-range))))
	      ;; this is basically the phase-offset	      
	      (setf (gethash (event-source event) (lastval o)) new-value)	      
	      (setf (slot-value event (modified-property o)) new-value)))
	(filter-events o events))
  events)

(defun stream-oscillate-between (name param upper-boundary lower-boundary &key cycle type
									    (affect-transition nil)
									    (keep-state t)
									    (track-state t)
									    (filter #'all-p)
									    (store nil))
  (let ((new-inst (make-instance 'stream-oscillate-between :mod-prop param :name name
				 :cycle cycle
				 :upper upper-boundary
				 :lower lower-boundary
				 :track-state track-state
				 :affect-transition affect-transition
				 :event-filter filter))
	(old-inst (gethash name *processor-directory*)))
    ;; if a current instance is replaced ...
    (when old-inst
      (setf (chain-bound new-inst) (chain-bound old-inst))
      (when keep-state
	(setf (pmod-step new-inst) (pmod-step (gethash name *processor-directory*)))
	(setf (lastval new-inst) (lastval (gethash name *processor-directory*)))))
    (when store
      (setf (gethash name *processor-directory*) new-inst))
    new-inst))

;; shorthand 
(defun s-oscb (param upper-boundary lower-boundary &key (cyc 128) 
						     (at nil)
						     (keep-state t)
						     (track-state t)
						     (f #'all-p)
						     (store nil)
						     (id nil))  
  (let ((s-osc-name (if id
			id
			(gensym))))
    (stream-oscillate-between s-osc-name param upper-boundary lower-boundary
			      :cycle cyc
			      :affect-transition at
			      :keep-state keep-state
			      :track-state track-state
			      :filter f
			      :store store
			      )))

(defclass chance-combine (stream-event-processor)
  ((event-to-combine :accessor event-to-combine :initarg :event-to-combine)
   (combi-chance :accessor combi-chance :initarg :combi-chance)
   (step :accessor pmod-step :initform 0)))

;; make combi-chance accessible to live modifications
;; the create-accessor only works within a macro ... pretty hacky, all in all ... 
(eval (create-accessor 'chance-combine 'combi-chance 'combi-chance))

;; make state-tracking switchable ???
(defmethod apply-self ((c chance-combine) events &key)
  (mapcar #'(lambda (event)	    
	    (let ((chance-val (random 100)))
	      (if (and (funcall (event-filter c) event)
		       (< chance-val (combi-chance c)))		  
		  (combine-single-events (event-to-combine c) event)		  
		  event)))        
	events))

;; the constructor ... if store is set, it'll be stored in the processor directory,
;; if not, it'll be stored in the processor directory by the chain building
;; routine
(defun chance-combine (name chance event &key (affect-transition nil) (filter #'all-p) (store nil))
  (let ((new-inst (make-instance 'chance-combine
				 :name name
				 :combi-chance chance
				 :event-to-combine event
				 :track-state nil
				 :mod-prop nil
				 :affect-transition affect-transition
				 :event-filter filter))
	(old-inst (gethash name *processor-directory*)))
    (when old-inst
      (setf (chain-bound new-inst) (chain-bound old-inst)))
    (when store
      (setf (gethash name *processor-directory*) new-inst))
    new-inst))

;; shorthand
(defun cc (chance event &key (at nil) (f #'all-p) (id nil))
  (let ((cc-name (if id
		     id
		     (gensym))))
    ;;(incudine::msg info "~D" cc-name)
    (chance-combine cc-name chance event :affect-transition at :filter f)))

;; a random walk on whatever parameter ...
(defclass stream-brownian-motion
    (stream-event-processor generic-brownian-motion) ())
  
;; make state-tracking switchable ??? 
(defmethod apply-self ((b stream-brownian-motion) events &key)
  (mapc #'(lambda (event)	    
	    (let* ((current-value (get-current-value b event))
		   (new-value (cap b
				   (+ current-value
				      (* (nth (random 2) '(-1 1))
					 (pmod-step-size b))))))
	      (setf (gethash (event-source event) (lastval b)) new-value)
	      (setf (slot-value event (modified-property b)) new-value)))
	(filter-events b events))
  events)

;; modifying ... always check if the modifier is already present !
(defun stream-brownian-motion (name param &key step-size wrap limit ubound lbound
					    (affect-transition nil) (keep-state t)
					    (track-state t) (filter #'all-p) (store nil))
  (let ((new-inst (make-instance 'stream-brownian-motion :step-size step-size :mod-prop param
				 :name name
				 :upper ubound
				 :lower lbound
				 :is-bounded limit
				 :is-wrapped wrap
				 :track-state track-state
				 :affect-transition affect-transition
				 :event-filter filter))
	(old-inst (gethash name *processor-directory*)))    
    (when old-inst
      (setf (chain-bound new-inst) (chain-bound old-inst))
      (when keep-state
	(setf (lastval new-inst) (lastval (gethash name *processor-directory*)))))
    (when store
      (setf (gethash name *processor-directory*) new-inst))
    new-inst))

;; shorthand
(defun s-brow (param lower upper &key (step 0.01)
					(wrap t)
					(limit nil) 
					(at nil)
					(keep-state t)
					(track-state t)
					(f #'all-p)
					(store nil)
					(id nil))
  (let ((s-brow-name (if id
			 id
			 (gensym))))
    (stream-brownian-motion s-brow-name param
			    :step-size step
			    :wrap wrap
			    :limit limit
			    :ubound upper
			    :lbound lower
			    :affect-transition at
			    :filter f
			    :track-state track-state
			    :keep-state keep-state
			    :store store)))

(defclass freeze-growth (stream-event-processor)
  ((act :accessor freeze-growth-act :initarg :act)))

(defun freeze (act)
  (make-instance 'freeze-growth :act act
		 :name (gensym)
		 :mod-prop nil		 
		 :affect-transition nil
		 :event-filter nil) )

;; make state-tracking switchable ??? 
(defmethod apply-self ((f freeze-growth) events &key)
  (if (freeze-growth-act f)
      (remove-if #'(lambda (event) (or (typep event 'growth-event)
			      (typep event 'shrink-event)
			      (typep event 'population-control-event)))
		 events)
      events))

(defclass inhibit-events (stream-event-processor)
  ((prob :accessor inhibit-events-prob :initarg :prob)))

;; make inhibit accessible to live modifications
;; the create-accessor only works within a macro ... pretty hacky, all in all ... 
(eval (create-accessor 'inhibit-events 'inhibit-events-prob 'prob))

(defmacro inh (prob filter)
  (let ((filter-name (intern (concatenate 'string (symbol-name filter) "-" (symbol-name 'p)))))
    `(funcall (lambda () (make-instance 'inhibit-events 
				   :prob ,prob
				   :name (gensym)
				   :mod-prop nil		 
				   :affect-transition nil
				   :event-filter #',filter-name)))))

;; make state-tracking switchable ??? 
(defmethod apply-self ((i inhibit-events) events &key) 
  (if (< (random 100) (inhibit-events-prob i))
      (remove-if (event-filter i) events)
      events))


(defclass exhibit-events (stream-event-processor)
  ((prob :accessor exhibit-events-prob :initarg :prob)))

;; make inhibit accessible to live modifications
;; the create-accessor only works within a macro ... pretty hacky, all in all ... 
(eval (create-accessor 'exhibit-events 'exhibit-events-prob 'prob))

(defmacro exh (prob filter)
  (let ((filter-name (intern (concatenate 'string (symbol-name filter) "-" (symbol-name 'p)))))
    `(funcall (lambda () (make-instance 'exhibit-events 
				   :prob ,prob
				   :name (gensym)
				   :mod-prop nil		 
				   :affect-transition nil
				   :event-filter #',filter-name)))))

;; make state-tracking switchable ??? 
(defmethod apply-self ((e exhibit-events) events &key) 
  (if (< (random 100) (exhibit-events-prob e))
      (remove-if-not (event-filter e) events)
      events))

(defclass parameter-limiter (stream-event-processor)
  ((upper :accessor limiter-upper-limit :initarg :upper)
   (lower :accessor limiter-lower-limit :initarg :lower)))

(defun lim (param lower upper &key (f #'all-p))
  (make-instance 'parameter-limiter
		 :name (gensym)
		 :mod-prop param		 
		 :affect-transition nil
		 :event-filter f
		 :upper upper
		 :lower lower))

(defmethod apply-self ((p parameter-limiter) events &key)
  (mapc #'(lambda (event)	    
	    (let* ((current-value (get-current-value p event)))	      
	      (setf (slot-value event (modified-property p))
		    (cond ((< current-value (limiter-lower-limit p)) (limiter-lower-limit p))
			  ((> current-value (limiter-upper-limit p)) (limiter-upper-limit p))
			  (t current-value)))))
	(filter-events p events))
  events)

(defclass parameter-wrapper (stream-event-processor)
  ((upper :accessor wrapper-upper-limit :initarg :upper)
   (lower :accessor wrapper-lower-limit :initarg :lower)))

(defun wrap (param lower upper &key (f #'all-p))
  (make-instance 'parameter-wrapper
		 :name (gensym)
		 :mod-prop param		 
		 :affect-transition nil
		 :event-filter f
		 :upper upper
		 :lower lower))

(defmethod apply-self ((w parameter-wrapper) events &key)
  (mapc #'(lambda (event)	    
	    (let* ((current-value (get-current-value w event)))	      
	      (setf (slot-value event (modified-property w))
		    (cond ((< current-value (wrapper-lower-limit w)) (wrapper-upper-limit w))
			  ((> current-value (wrapper-upper-limit w)) (wrapper-lower-limit w))
			  (t current-value)))))
	(filter-events w events))
  events)
