;; this file contains all the language functions, serving as the interface to the user ...
;; (unless those that are directly provided by, say, event definition ...)
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
			    (affect-transition nil)
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
					       :affect-transition ,affect-transition
					       :combine-filter ,combine-filter))))
		 ,name)))

;; replace the content (or parts of the content) of a graph ...
(defun graph-replace (name new-content)
  (let ((current-graph (source-graph (gethash name *processor-directory*))))
    (mapc #'(lambda (obj)
	    (cond ((typep obj 'edge) (insert-edge current-graph obj))
		  ((typep obj 'node) (insert-node current-graph obj))))
	  new-content)
    (setf (source-graph (gethash name *processor-directory*)) current-graph)))

;; only for single values (pitch, duration, level etc )
(defmacro values->graph (name event-type values
			 &key (type 'loop) (combine-mode 'append) (affect-transition nil) (randomize 0))
  `(funcall #'(lambda () (let ((new-graph (make-instance 'graph))
	(count 1))		      
    (setf (graph-id new-graph) ,name)
    (mapc #'(lambda (value)	      
	      (insert-node new-graph (node count (,event-type value)))
	      (if (> count 1)
		  (insert-edge new-graph (edge (- count 1) count :prob 100)))
	      (incf count)
	      ) ,values)
    ;; reverse last step
    (decf count)
    (if (eq ',type 'loop)
	(insert-edge new-graph (edge count 1 :prob 100)))
    ;; tbd: make dependent on randomize factor    
    (loop for src from 1 to count
       do (loop for dest from 1 to count
	     do (let ((randval (random 100)))
		 (if (and (< randval ,randomize) (not (get-edge new-graph src dest)))
		     (insert-edge new-graph
				  (edge src dest :prob 0))))))
    (if (gethash ,name *processor-directory*)
	(setf (source-graph (gethash ,name *processor-directory*)) new-graph)
	(setf (gethash ,name *processor-directory*)
	      (make-instance 'graph-event-processor :name ,name
			     :graph new-graph :copy-events t
			     :current-node 1 :combine-mode ,combine-mode
			     :affect-transition ,affect-transition
			     :combine-filter #'all-p)))))))

;; only for single values (pitch, duration, level etc )
;; takes a list of values and transition times and turns them into a graph
;; filled with single-value events like (pitch ..) or (lvl ..)
(defmacro values->transitions->graph (name event-type values transitions
				      &key (type 'loop) (randomize 0) (combine-mode 'append) (affect-transition nil))
  `(funcall #'(lambda ()
		(let ((new-graph (make-instance 'graph))
		      (count 1)
		      (len (list-length ,values)))		      
    (setf (graph-id new-graph) ,name)
    (mapc #'(lambda (value transdur)	      
	      (insert-node new-graph (node count (,event-type value)))
	      (if (< count len)
		  (insert-edge new-graph (edge count (+ count 1) :prob 100 :dur transdur)))
	      (incf count)
	      ) ,values ,transitions)
    ;; reverse last step
    (decf count)
    (if (eq ',type 'loop)
	(insert-edge new-graph (edge count 1 :prob 100 :dur (car (reverse ,transitions)))))
    ;; tbd: make dependent on randomize factor    
    (loop for src from 1 to count
       do (loop for dest from 1 to count
	     do (let ((randval (random 100)))
		 (if (and (< randval ,randomize) (not (get-edge new-graph src dest)))
		     (insert-edge new-graph
				  (edge src dest :prob 0))))))
    (if (gethash ,name *processor-directory*)
	(setf (source-graph (gethash ,name *processor-directory*)) new-graph)
	(setf (gethash ,name *processor-directory*)
	      (make-instance 'graph-event-processor :name ,name
			     :graph new-graph :copy-events t
			     :current-node 1 :combine-mode ,combine-mode
			     :affect-transition ,affect-transition
			     :combine-filter #'all-p)))))))

;; takes notes in the format '(pitch duration) ant turns them into a loop graph
;; which might be randomized
(defun notes->midi-graph (name &key notes (level 0.5) (type 'loop) (randomize 0) (default-dur 512))
  (let ((new-graph (make-instance 'graph))
	(count 1)
	(len (list-length notes)))		      
    (setf (graph-id new-graph) name)
    (mapc #'(lambda (note)	      
	      (insert-node new-graph (node count (mid (car note) :lvl level :dur (- (cadr note) 10))))
	      (if (< count len)
		  (insert-edge new-graph (edge count (+ count 1) :prob 100 :dur (cadr note))))
	      (incf count)) notes)
    ;; reverse last step
    (decf count)
    (if (eq type 'loop)
	(insert-edge new-graph (edge count 1 :prob 100 :dur (cadr (car (reverse notes))))))
    ;; tbd: make dependent on randomize factor    
    (loop for src from 1 to count
       do (loop for dest from 1 to count
	     do (let ((randval (random 100)))
		 (if (and (< randval randomize) (not (get-edge new-graph src dest)))
		     (insert-edge new-graph
				  (edge src dest :prob 0 :dur default-dur))))))
    (if (gethash name *processor-directory*)
	(setf (source-graph (gethash name *processor-directory*)) new-graph)
	(setf (gethash name *processor-directory*)
	      (make-instance 'graph-event-processor :name name
			     :graph new-graph :copy-events t
			     :current-node 1 :combine-mode 'append
			     :combine-filter #'all-p)))))

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
(defun stream-brownian-motion (name param &key step-size wrap limit ubound lbound
				     (affect-transition nil) (keep-state t)
				     (track-state t) (filter #'all-p))
  (let ((new-inst (make-instance 'stream-brownian-motion :step-size step-size :mod-prop param
				 :name name
				 :upper ubound
				 :lower lbound
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

(defun stream-oscillate-between (name param upper-boundary lower-boundary &key cycle type
								     (affect-transition nil)
								     (keep-state t) (track-state t)
								     (filter #'all-p))
  (let ((new-inst (make-instance 'stream-oscillate-between :mod-prop param :name name
				 :cycle cycle
				 :upper upper-boundary
				 :lower lower-boundary
				 :track-state track-state
				 :affect-transition affect-transition
				 :event-filter filter)))
    ;; if a current instance is replaced ...
    (when (gethash name *processor-directory*)
      (setf (is-active new-inst) t)
      (when keep-state
	(setf (pmod-step new-inst) (pmod-step (gethash name *processor-directory*)))
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

;; deactivate ... if it's a modifying event processor, delete it ... 
(defun deactivate (event-processor-id &key (del nil))
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

(defun stop ()
  (loop for proc being the hash-keys of *processor-directory*
     do (deactivate proc)))


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
		      (funcall fun d2))
		    ;; note off
		    (when (eql st 128)
		      (when off
			(funcall off d2))
		      ;; toggle light
		      (when toggle
			(if (gethash pad-id *pad-toggle-states*)
			    (setf (gethash pad-id *pad-toggle-states*) nil)
			    (progn			       
			      (setf (gethash pad-id *pad-toggle-states*) t)
			      (jackmidi:write cm::*midiout*
					      (coerce `(144 ,pad-id 96) 'jackmidi:data)))))))))))
    (when (gethash pad-id *midi-responders*)
      (incudine::remove-responder (gethash pad-id *midi-responders*)))
	 (setf (gethash pad-id *midi-responders*) resp)))

(defun clear-midi-responders ()
  (labels ((rem-resp (key responder)
	     (incudine::remove-responder responder)))
    (maphash #'rem-resp *midi-responders*)))

(defun midi->range (midi-val range)
  (car (multiple-value-list (round (* range (/ midi-val 127))))))  

;; should i find a better name for this function ??
(defun pring (graph &optional stream)
  (format stream "~a" (print-graph (gethash graph *processor-directory*))))

(defun graph->code (graph file)
  (with-open-file (out-stream file :direction :output :if-exists :supersede)
   (format out-stream "~a" (print-graph (gethash graph *processor-directory*)))))

(defun graph->svg (graph file &key (renderer 'circo))
  (with-open-file (out-stream file :direction :output :if-exists :supersede)
    (graph->dot (source-graph (gethash graph *processor-directory*)) :output out-stream))
  (cond ((eq renderer 'dot)
	 (sb-ext:run-program "/usr/bin/dot" (list "-T" "svg" "-O" file "-Gnslimit" "-Gnslimit1")))
	((eq renderer 'neato)
	 (sb-ext:run-program "/usr/bin/neato" (list "-T" "svg" "-O"
						    file
						    "-Goverlap=scalexy -Gnodesep=0.6 -Gstart=0.5" )))
	((eq renderer 'circo)
	 (sb-ext:run-program "/usr/bin/circo" (list "-T" "svg" "-O" file)))
	((eq renderer 'sfdp)
	 (sb-ext:run-program "/usr/bin/sfdp" (list "-T" "svg" "-O" file
						   "-Goverlap=scalexy -Gnodesep=0.6 -Gstart=0.5")))
	((eq renderer 'twopi)
	 (sb-ext:run-program "/usr/bin/twopi" (list "-T" "svg" "-O" file "-Goverlap=scalexy")))))
