;; stateful parameter modifier ... (yes, really ...)
;; every one of those needs an "evaluate" function ...
(defclass param-mod-object ()
  ((step :accessor pmod-step :initform 0)
   (time :accessor pmod-time :initform 0)
   (current-value :accessor pmod-current-value :initarg :current-value)))

;; before each evaluation, set time ...
(defmethod evaluate :before ((p param-mod-object) &key)
  (setf (pmod-time p) (incudine:now)))

;; after each evaluation, increment step counter
(defmethod evaluate :after ((p param-mod-object) &key)
  (incf (pmod-step p)))

;; this one is stateless, not dependent on current value ...
(defclass param-oscillate-between (param-mod-object))

(defmethod evaluate ((o param-oscillate-between))
  (let* ((osc-range (- upper lower))		   
	    (degree-increment (/ 360 cycle o))
	    (degree (mod (* degree-increment (mod step cycle)) 360))
	 (abs-sin (abs (sin (radians degree)))))    
    (+ lower (* abs-sin osc-range))))

(defclass generic-brownian-motion ()
  (upper-boundary :accessor ubound :initarg :upper-boundary)
  (lower-boundary :accessor lbound :initarg :lower-boundary)
  (step-size :accessor step-size :initarg :step-size)
  (is-bounded :accessor is-bounded :initarg :is-bounded)
  (is-wrapped :accessor is-wrapped :initarg :is-wrapped))

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
(defclass param-brownian-motion (generic-brownian-motion param-mod-object))

(defmethod evaluate ((b param-brownian-motion))
  (let* ((new-value (cap b (+ (pmod-current-value b) 
			      (* (nth (random 2) '(-1 1)) (step-size b))))))
    ;; stateful - don't forget to set value ! 
    (setf (pmod-current-value b) new-value)
    ;; return new value
    new-value))

;; the atomic units of music - event and transition ...
(defclass event ()
  ((source :accessor event-source)
   (tags :accessor event-tags :initarg :tags)
   (backends :accessor event-backends :initarg :backends :initform `(,*default-dsp-backend*))
   (value-combine-function :accessor value-combine-function
			   :initarg :combi-fun :initform #'replace-value)))

;; the default value combination function
(defun replace-value (b a) a)

;; will be the accumulator ... 
(defclass incomplete-event (event) ())

;; those "abstract" events provide the building blocks
;; for the events that will later on produce a sound 
(defclass string-event (event)
    ((msg :accessor event-message :initarg :msg)))

(defclass pitch-event (event)
  ((pitch :initarg :pitch)))


(defun add-slot-to-class (class name &key (initform nil) accessors readers writers
				       initargs (initfunction (constantly nil)))
  (check-type class symbol)
  (let ((new-slots (list (list :name name
                               :readers (union accessors readers)
                               :writers (union writers
                                               (mapcar #'(lambda (x)
                                                           (list 'setf x))
                                                       accessors)
                                               :test #'equal)
                               :initform initform
                               :initargs initargs
                               :initfunction initfunction))))
    (dolist (slot-defn (class-direct-slots (find-class class)))
      (push (direct-slot-defn->initarg slot-defn)
            new-slots))
    (ensure-class class :direct-slots new-slots)))

;; macro to faciliate defining events
;; defines the event class, the language constructor, and the
;; value accessor function ...
(defmacro define-event (event-long-name
			event-short-name
			parent-events
			parameters)
  (let* ((class-name (intern (format nil "~a-event" event-long-name) 'megra))
	 (constructor-name (intern (format nil "~a" event-short-name) 'megra))
	 )

    

  `(progn
     (defclass ,class-name ,parent-events)
     (loop for param in parameters
	do ((let* (())
	      (add-slot-to-class )

	      )
	    (defgeneric name-access (foo)
	      (:method ((foo foo))
		(format t "~&Getting name.~%")
		(slot-value foo 'name)))

	    (defgeneric (setf name-access) (name foo)
	      (:method (name (foo foo))
		(format   t "~&Setting a new name.~%")
		(setf (slot-value foo 'name) name))
	    
	    ))

     (defun lvl (lvl &key (tags nil) (combi-fun #'replace-value))
       (make-instance 'level-event :lvl lvl :tags tags :combi-fun combi-fun))


     ))
  )


(defmethod event-get-pitch ((e event))
  
  (if (typep (slot-value e 'pitch) 'function)
      (funcall (slot-value e 'pitch) )
      (slot-value e 'pitch)
      )
  )

(define-event 'duration 'dur '((duration dur) )  )

(defclass duration-event (event)
  ((dur :accessor event-duration :initarg :dur)))

(defclass instrument-event (event)
  ((inst :accessor event-instrument :initarg :inst)))

(defclass rate-event (event)
  ((rate :accessor event-rate :initarg :rate)))

(defclass attack-event (event)
  ((atk :accessor event-attack :initarg :atk)))

(defclass release-event (event)
  ((rel :accessor event-release :initarg :rel)))

(defclass start-event (event)
  ((start :accessor event-start :initarg :start)))

(defclass filter-hp-event (event)
  ((hp-freq :accessor event-hp-freq :initarg :hp-freq)
   (hp-q :accessor event-hp-q :initarg :hp-q)))

(defclass filter-peak-event (event)
  ((pf-freq :accessor event-pf-freq :initarg :pf-freq)
   (pf-q :accessor event-pf-q :initarg :pf-q)
   (pf-gain :accessor event-pf-gain :initarg :pf-gain)))

(defclass filter-lp-event (event)
  ((lp-freq :accessor event-lp-freq :initarg :lp-freq)
   (lp-q :accessor event-lp-q :initarg :lp-q)
   (lp-dist :accessor event-lp-dist :initarg :lp-dist)))

(defclass reverb-event (event)
  ((rev :accessor event-reverb :initarg :rev)))

;; ready for ambisonics
;; pos is the simple stereo position,
;; azimuth, elevation and distance the ambisonics parameters
(defclass spatial-event (event)
  ((pos :accessor event-position :initarg :pos)
   (azi :accessor event-azimuth :initarg :azi)
   (ele :accessor event-elevation :initarg :ele)
   (dist :accessor event-distance :initarg :dist)
   (ambi-p :accessor event-ambi-p :initarg :ambi-p :initform nil)))

(defclass tuned-instrument-event (pitch-event instrument-event level-event duration-event) ())

(defclass midi-event (tuned-instrument-event) ())

(defclass grain-event (level-event duration-event spatial-event start-event rate-event
				   attack-event release-event filter-hp-event filter-lp-event
				   filter-peak-event reverb-event)
  ((sample-folder :accessor sample-folder :initarg :sample-folder)
   (sample-file :accessor sample-file :initarg :sample-file)
   (sample-location :accessor sample-location)))

;;(in-package :megra)
(defclass gendy-event (level-event duration-event filter-lp-event attack-event
				   release-event reverb-event spatial-event)
  ((adstr :accessor event-amp-distr :initarg :adstr)
   (ddstr :accessor event-dur-distr :initarg :ddstr)
   (adstr-par :accessor event-amp-distr-param :initarg :adstr-par)
   (ddstr-par :accessor event-dur-distr-param :initarg :ddstr-par)
   (freq-min :accessor event-freq-min :initarg :freq-min)
   (freq-max :accessor event-freq-max :initarg :freq-max)
   (a-scl :accessor event-amp-scale :initarg :a-scl)
   (d-scl :accessor event-dur-scale :initarg :d-scl)))

(defmethod initialize-instance :after ((g grain-event) &key)
  (setf (sample-location g) (concatenate 'string *sample-root*
					 (sample-folder g) "/" (sample-file g) ".wav")))

;; special event that contains a control function to modify things or start/stop things ...
(defclass control-event (event)
  ((control-function :accessor control-function :initarg :control-function)))

;; slots are equal if their name is equal ... period.
(defun slot-eq (a b)
  (eq (slot-definition-name a) (slot-definition-name b)))

(defmethod event-has-slot ((e event) slot &key)
  (member slot (class-slots (class-of e)) :test 'slot-eq))

(defmethod event-has-slot-by-name ((e event) slot-name  &key)
  (member slot-name (mapcar #'slot-definition-name (class-slots (class-of e)))))

;; not quite sure why this works, but it does ... 
;; http://stackoverflow.com/questions/17002816/lisp-clos-adding-a-slot-to-the-process-class
(defun direct-slot-defn->initarg (slot-defn)
  (list :name (slot-definition-name slot-defn)
        :readers (slot-definition-readers slot-defn)
        :writers (slot-definition-writers slot-defn)
        :initform (slot-definition-initform slot-defn)
        :initargs (slot-definition-initargs slot-defn)
        :initfunction (slot-definition-initfunction slot-defn)))


;; check if event b has all slots that event a has
(defmethod events-compatible ((a event) (b event) &key)
  (subsetp (class-slots (class-of a)) (class-slots (class-of b)) :test 'slot-eq))

;; the slots of the basic event class should of course not be overwritten ...
;; manual makeshift solution
(defparameter *protected-slots* '(source value-combine-function tags backends))

(defmethod overwrite-slots ((a event) (b event) &key)
  (loop for slot in (class-slots (class-of a))
     do (when (slot-boundp-using-class (class-of b) b slot)
	  (unless (member (slot-definition-name slot) *protected-slots*)
	    (setf (slot-value b (slot-definition-name slot))
		  (funcall (value-combine-function a) (slot-value b (slot-definition-name slot))
			   (slot-value a (slot-definition-name slot))))))) b)

(defmethod copy-slots-to-class ((a event) (b event) &key)
  (loop for slot in (class-direct-slots (class-of a))
     do (unless (event-has-slot b slot)
	  (add-slot-to-class (class-name (class-of b)) (slot-definition-name slot)
			     :readers (slot-definition-readers slot)
			     :writers (slot-definition-writers slot)))))

;; a overwrites b, b (or incomplete) is returned ...
(defmethod combine-single-events ((a event) (b event) &key)
  (cond ((events-compatible a b) (overwrite-slots a b))
	;; merge events into a new incomplete event
	(t (let ((new-event (make-instance 'incomplete-event)))
	      (copy-slots-to-class a new-event)
	      (copy-slots-to-class b new-event)
	      (overwrite-slots b new-event)
	      (overwrite-slots a new-event)
	      ))))

;; combining events ... a has precedence
(defmethod combine-events (events-a events-b &key (mode 'append) (filter #'all-p))
  (cond ((eq mode 'append) (append events-a events-b))
	((eq mode 'zip) (let ((filtered-and-combined
			       (mapcar #'combine-single-events events-a
				       (remove-if-not filter events-b)))
			      (rest (remove-if filter events-b)))
			  (append filtered-and-combined rest)))))

;; it might seem weird to treat the transition as an event, but it makes lots
;; of things easier, and musically it's sound to treat the space between events
;; as a special type of event ... i think ...
(defclass transition (event)
    ((dur :accessor transition-duration :initarg :dur)))



				        



