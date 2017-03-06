;; the atomic units of music - event and transition ...
(defclass event ()
  ((source :accessor event-source)
   (tags :accessor event-tags :initarg :tags)
   (backends :accessor event-backends :initarg :backends :initform `(,*default-dsp-backend*))
   (value-combine-function :accessor value-combine-function
			   :initarg :combi-fun :initform #'replace-value)))

;; the default value combination function
(defun replace-value (a b) a)

;; will be the accumulator ... 
(defclass incomplete-event (event) ())

;; those "abstract" events provide the building blocks
;; for the events that will later on produce a sound 
(defclass string-event (event)
    ((msg :accessor event-message :initarg :msg)))

(defclass pitch-event (event)
  ((pitch :accessor event-pitch :initarg :pitch)))

(defclass level-event (event)
  ((lvl :accessor event-level :initarg :lvl)))

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



				        



