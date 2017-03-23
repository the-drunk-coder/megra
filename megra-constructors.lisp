;; structural

(defun node (id &rest content)
  (make-instance 'node :id id :content content :color 'white))

(defmacro node-col (id (&key (col ''white)) &body content)
  `(make-instance 'node :id ,id :content (list ,@content) :color ,col))

(defun edge (src dest &key prob (dur 512))
  (make-instance 'edge :src src :dest dest :prob prob :content `(,(make-instance 'transition :dur dur))))

;; this macro is basically just a wrapper for the (original) function,
;; so that i can mix keyword arguments and an arbitrary number of
;; ensuing graph elements ... 
(defmacro graph (name (&key (perma nil) (combine-mode ''append)
			    (combine-filter #'all-p)) &body graphdata)
  `(funcall #'(lambda () (let ((new-graph (make-instance 'graph)))		      
		      (setf (graph-id new-graph) ,name)    
		      (mapc #'(lambda (obj)
				(cond ((typep obj 'edge) (insert-edge new-graph obj))
				      ((typep obj 'node) (insert-node new-graph obj))))
			    (list ,@graphdata))
		      (if (gethash ,name *processor-directory*)
			  (setf (source-graph (gethash ,name *processor-directory*)) new-graph)
			  (setf (gethash ,name *processor-directory*)
				(make-instance 'graph-event-processor :name ,name
					       :graph new-graph :copy-events (not ,perma)
					       :current-node 1 :combine-mode ,combine-mode
					       :combine-filter ,combine-filter))))
		 ,name)))

;; replace the content (or parts of the content) of a graph ...
(defun graph-replace (name new-content)
  (let ((current-graph (source-graph (gethash name *processor-directory*))))
    (princ new-content)
    (mapc #'(lambda (obj)
	    (cond ((typep obj 'edge) (insert-edge current-graph obj))
		  ((typep obj 'node) (insert-node current-graph obj))))
	  new-content)
    (setf (source-graph (gethash name *processor-directory*)) current-graph)))

;; build the event processor chain, in the fashion of a douby-linked list ...
(defun connect (processor-ids)
  (when (cadr processor-ids)
    (setf (successor (gethash (car processor-ids) *processor-directory*))
	  (gethash (cadr processor-ids) *processor-directory*))
    (setf (predecessor (gethash (cadr processor-ids) *processor-directory*))
	  (gethash (car processor-ids) *processor-directory*))    
    (connect (cdr processor-ids))))

;; ensure uniqueness by detaching event processors (will be re-attached if necessary)
;; and deactivate all those currently not needed ...
(defun detach (processor current-processor-ids)
  (when (predecessor processor)
    (detach (predecessor processor) current-processor-ids)
    (setf (predecessor processor) nil))
  (when (successor processor)
    (setf (successor processor) nil))
  (when (not (member (name processor) current-processor-ids))
    (princ (name processor))
    (deactivate (name processor))))

;; dispatching ... one dispatcher per active event processor ...
;; if 'unique' is t, an event processor can only be hooked into
;; one chain.
(defmacro dispatch ((&key (unique t) (chain nil)) &body proc-body)
  `(funcall #'(lambda () (let ((event-processors (list ,@proc-body)))		      
		      (when (and ,unique (not ,chain))
			(detach (gethash (car (last event-processors))
					 *processor-directory*) event-processors)) 
		      (when (not ,chain)
			(connect event-processors))		      
		      ;; if the first event-processor is not active yet,
		      ;; create a dispatcher to dispatch it ... 
		      (unless (is-active (gethash (car event-processors) *processor-directory*))
			(let ((dispatcher (make-instance 'event-dispatcher)))
			  (activate (car event-processors))
			  (perform-dispatch dispatcher (car event-processors) (incudine:now))))))))

;; chain events without dispatching ...
(defmacro chain ((&key (unique t)) &body proc-body)
  `(funcall #'(lambda () (let ((event-processors (list ,@proc-body)))
		      (when ,unique 
			(detach (gethash (car (last event-processors))
					 *processor-directory*) event-processors))
		      (connect event-processors)))))


(defun toggle (proc)
  (if (is-active (gethash proc *processor-directory*))
      (deactivate proc :del nil)
      (dispatch (:chain t) proc)))

;; modifying ... always check if the modifier is already present !
(defun brownian-motion (name param &key step-size wrap limit ubound lbound
				     (affect-transition nil) (keep-state t)
				     (track-state t) (filter #'all-p))
  (let ((new-inst (make-instance 'brownian-motion :step-size step-size :mod-prop param :name name
				 :upper-boundary ubound
				 :lower-boundary lbound
				 :is-bounded limit
				 :is-wrapped wrap
				 :track-state track-state
				 :affect-transition affect-transition
				 :event-filter filter)))    
    (when (gethash name *processor-directory*)
      (setf (is-active new-inst) t)
      (when keep-state
	(setf (lastval new-inst) (lastval (gethash name *processor-directory*)))))
    (setf (gethash name *processor-directory*) new-inst))
  name)

(defun oscillate-between (name param upper-boundary lower-boundary &key cycle type
								     (affect-transition nil)
								     (keep-state t) (track-state t)
								     (filter #'all-p))
  (let ((new-inst (make-instance 'oscillate-between :mod-prop param :name name
				 :cycle cycle
				 :upper-boundary upper-boundary
				 :lower-boundary lower-boundary
				 :track-state track-state
				 :affect-transition affect-transition
				 :event-filter filter)))
    ;; if a current instance is replaced ...
    (when (gethash name *processor-directory*)
      (setf (is-active new-inst) t)
      (when keep-state
	;;(princ "keep-state")
	(setf (step-count new-inst) (step-count (gethash name *processor-directory*)))
	(setf (lastval new-inst) (lastval (gethash name *processor-directory*)))))
    (setf (gethash name *processor-directory*) new-inst))
  name)

(defun spigot (name &key flow)
  (let ((new-inst (make-instance 'spigot :flow flow :name name)))
    (when (gethash name *processor-directory*)
      (setf (is-active new-inst) t))
    (setf (gethash name *processor-directory*) new-inst))
  name)

(defun chance-combine (name chance event &key (affect-transition nil) (filter #'all-p))
  (let ((new-inst (make-instance 'chance-combine
				 :name name
				 :combi-chance chance
				 :event-to-combine event
				 :track-state nil
				 :mod-prop nil
				 :affect-transition affect-transition
				 :event-filter filter)))
    (when (gethash name *processor-directory*)
      (setf (is-active new-inst) t))
    (setf (gethash name *processor-directory*) new-inst))
  name)

;; events
(defun string-event (msg)
  (make-instance 'string-event :msg msg :tags nil))

(defun mid (pitch &key dur lvl (tags nil) (combi-fun #'replace-value))
  (make-instance 'midi-event :pitch pitch :lvl lvl :dur dur :tags tags :combi-fun combi-fun))

(defun grain (folder file &key
			    (tags nil)
			    (dur 256)
			    (lvl 0.5)
			    (pos 0.5)
			    (start 0.0)
			    (rate 1.0)
			    (hp-freq 10)
			    (hp-q 0.4)
			    (pf-freq 1000)
			    (pf-q 10)
			    (pf-gain 0.0)
			    (lp-freq 19000)
			    (lp-q 0.4)
			    (lp-dist 0.0)
			    (atk 7)
			    (rel 7)
			    (rev 0.0)
			    (azi 0.0)
			    (ele 0.0)
			    (ambi nil)
			    (combi-fun #'replace-value))
  (make-instance 'grain-event :lvl lvl :dur dur :start start :pos pos :hp-freq hp-freq
		 :rate rate :hp-freq hp-freq :hp-q hp-q
		 :pf-freq pf-freq :pf-q pf-q :pf-gain pf-gain
		 :lp-freq lp-freq :lp-q lp-q :lp-dist lp-dist :rev rev
		 :atk atk :rel rel :sample-folder folder :sample-file file :tags tags :azi azi
		 :ele ele :dist 1.0 :ambi-p ambi :combi-fun combi-fun))

(defun ctrl (ctrl-fun &key (tags nil))
  (make-instance 'control-event :control-function ctrl-fun :tags tags))

(defun dur (dur &key (tags nil) (combi-fun #'replace-value))
  (make-instance 'duration-event :dur dur :tags tags :combi-fun combi-fun))

(defun lvl (lvl &key (tags nil) (combi-fun #'replace-value))
  (make-instance 'level-event :lvl lvl :tags tags :combi-fun combi-fun))

(defun pitch (pitch &key (tags nil) (combi-fun #'replace-value))
  (make-instance 'pitch-event :pitch pitch :tags tags :combi-fun combi-fun))

(defun pos (pos &key (azi 0) (ele 0) (dist 0) (tags nil) (combi-fun #'replace-value))
  (make-instance 'spatial-event :pos pos :azi azi :ele ele
		 :dist dist :tags tags :combi-fun combi-fun))

(defun ambi-pos (azi ele &key (dist 0) (tags nil) (combi-fun #'replace-value))
  (make-instance 'spatial-event :pos 0.0 :azi azi :ele ele
		 :dist dist :tags tags :ambi-p t :combi-fun combi-fun))

(defun rate (rate &key (tags nil) (combi-fun #'replace-value))
  (make-instance 'rate-event :rate rate :tags tags :combi-fun combi-fun))

(defun start (start &key (tags nil) (combi-fun #'replace-value))
  (make-instance 'start-event :start start :tags tags :combi-fun combi-fun))

;; deactivate ... if it's a modifying event processor, delete it ... 
(defun deactivate (event-processor-id &key (del t))
  (setf (is-active (gethash event-processor-id *processor-directory*)) nil)
  ;; this is as un-functional as it gets, but anyway ...
  (if (and del
	   (or (typep (gethash event-processor-id *processor-directory*) 'modifying-event-processor)
	       (typep (gethash event-processor-id *processor-directory*) 'spigot)))
      (setf (gethash event-processor-id *processor-directory*) nil)))

(defun activate (event-processor-id)
  (setf (is-active (gethash event-processor-id *processor-directory*)) t))

(defun clear ()
  (setf *processor-directory* (make-hash-table :test 'eql)))

;; convenience functions to set params in some object ...
(defun pset (object param value)
  (setf (slot-value (gethash object *processor-directory*) param) value))

;; controller stuff for akai lpd8
(defun register-knob (knob fun)
  (let ((resp (incudine::make-responder cm::*midiin*
					(lambda (st d1 d2)
					  (when (eql d1 knob)
					    (funcall fun d2))))))
    (when (gethash knob *midi-responders*)
      (incudine::remove-responder (gethash knob *midi-responders*)))
    (setf (gethash knob *midi-responders*) resp)))


(defun register-pad (pad fun &key (off nil) (toggle t))
  (let* ((pad-id (+ pad 35))
	 (resp (incudine::make-responder
		cm::*midiin*
		(lambda (st d1 d2)	   
		  (when (eql d1 pad-id)
		    ;; note on
		    (when (eql st 144)
		      (funcall fun d2)
		      ;; toggle light
		      (when toggle 
			(cond ((gethash pad-id *pad-toggle-states*)
			       (setf (gethash pad-id *pad-toggle-states*) nil))
			      ((not (gethash pad-id *pad-toggle-states*))			       
				 (setf (gethash pad-id *pad-toggle-states*) t)
				 (jackmidi:write cm::*midiout*
						 (coerce `(144 ,pad-id 96)
							 'jackmidi:data))))))
		      ;; note off
		    (when (and (eql st 128) off) (funcall off d2)))))))
    (when (gethash pad-id *midi-responders*)
      (incudine::remove-responder (gethash pad-id *midi-responders*)))
    (setf (gethash pad-id *midi-responders*) resp)))
  
  
