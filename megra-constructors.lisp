(in-package :megra)

;; this file once contained all the language functions, serving as the interface to the user,
;; but lateron i found it clearer to put the functions directly next to their backend stuff ...
;; (unless those that are directly provided by, say, event definition ...)
(defun node (id &rest content)
  (make-instance 'node :id id :content content :color 'white))

;; shorthand for node 
(defun n (id &rest content)
  (make-instance 'node :id id :content content :color 'white))

(defmacro node-col (id (&key (col ''white)) &body content)
  `(make-instance 'node :id ,id :content (list ,@content) :color ,col))

;; shorthand for node-col
(setf (macro-function 'n-c) (macro-function 'node-col))

(defun edge (src dest &key prob (dur 512))
  (make-instance 'edge :src src :dest dest :prob prob
		 :content `(,(make-instance 'transition :dur dur))))

;; shorthand for edge
(defun e (src dest &key p (d 512))
  (make-instance 'edge :src src :dest dest :prob p :content `(,(make-instance 'transition :dur d))))


(cadr '(0.0 4.0))
;; dummy 
(defun add-imprecision (orig
			imprecision
			&key
			  object-name
			  (min SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY)
			  (max SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY))
  (let* ((newval (+ orig (* (* (- 20000 (random 40000)) imprecision)
			    (/ orig 20000))))
	 (limits (gethash object-name *parameter-limits*))
	 (min-res (if (and limits (car limits))
		      (car limits)
		      min))
	 (max-res (if (and limits (cadr limits))
		      (cadr limits)
		      max)))
    ;;(format t "hi ~D ~D ~%" object-name newval)
    (cond ((< newval min-res) min-res)
	  ((> newval max-res) max-res)
	  (t newval))))

(defun deepcopy-list (list &key
			     (imprecision 0.0)
			     exclude-keywords
			     precise-keywords)
  (mapcar #'(lambda (thing)	      
	      (deepcopy thing
			:imprecision imprecision
			:exclude-keywords exclude-keywords
			:precise-keywords precise-keywords)) list))

(defun deepcopy-hash-table (orig &key (imprecision 0.0) exclude-keywords precise-keywords)
  (let ((new-table (make-hash-table :test (hash-table-test orig))))
    (loop for key being the hash-keys of orig
       do (setf (gethash key new-table)
		(deepcopy (gethash key orig)
			  :imprecision imprecision
			  :exclude-keywords exclude-keywords
			  :precise-keywords precise-keywords)))
    new-table))

(defmethod deepcopy-generic-object (object
				    &key (imprecision 0.0)
				      exclude-keywords
				      precise-keywords)
  (let ((copy (allocate-instance (class-of object))))
    (loop for slot in (class-slots (class-of object))
       do (when (slot-boundp-using-class (class-of object) object slot)
	    (setf (slot-value copy (slot-definition-name slot))
		  (cond
		    ((member (slot-definition-name slot) exclude-keywords) 
		     (slot-value object (slot-definition-name slot)))
		    ((member (slot-definition-name slot) precise-keywords) 
		     (deepcopy
		      (slot-value object (slot-definition-name slot))
		      :imprecision 0.0
		      :exclude-keywords exclude-keywords
		      :precise-keywords precise-keywords
		      :object-name (slot-definition-name slot)))
		    (t (deepcopy
			(slot-value object (slot-definition-name slot))
			:imprecision imprecision
			:exclude-keywords exclude-keywords
			:precise-keywords precise-keywords
			:object-name (slot-definition-name slot)))))))
    copy))

(defmethod deepcopy-object ((o standard-object)
			    &key (imprecision 0.0)
			      exclude-keywords
			      precise-keywords)
  (deepcopy-generic-object o
			   :imprecision imprecision
			   :exclude-keywords exclude-keywords
			   :precise-keywords precise-keywords))


(defmethod deepcopy-object ((n node) &key (imprecision 0.0)
				       exclude-keywords precise-keywords)
  (deepcopy-generic-object n
			   :imprecision imprecision
			   :exclude-keywords (append
					      exclude-keywords '(global-id id))
			   :precise-keywords precise-keywords))

(defmethod deepcopy-object ((e edge) &key (imprecision 0.0)
				       exclude-keywords precise-keywords)
  (deepcopy-generic-object e
			   :imprecision imprecision
			   :exclude-keywords (append exclude-keywords
						     '(source destination))
			   :precise-keywords (append precise-keywords
						     '(probability))))

(defmethod deepcopy-object ((tr transition)
			    &key (imprecision 0.0)
			      exclude-keywords
			      precise-keywords)
  (deepcopy-generic-object tr
			   :imprecision imprecision
			   :exclude-keywords  exclude-keywords
			   :precise-keywords (append precise-keywords
						     '(dur))))

(defmethod deepcopy-object ((g graph) &key (imprecision 0.0)
					exclude-keywords
					precise-keywords)
  (deepcopy-generic-object g
			   :imprecision imprecision
			   :exclude-keywords (append
					      exclude-keywords
					      '(id
						max-id
						highest-edge-order
						event-source))
			   :precise-keywords precise-keywords))

(defmethod deepcopy-object ((e event-processor) &key (imprecision 0.0)
						  exclude-keywords
						  precise-keywords)
  (deepcopy-generic-object e
			   :imprecision imprecision
			   :exclude-keywords (append
					      exclude-keywords
					      '(successor predecessor))
			   :precise-keywords precise-keywords))


(defmethod deepcopy (object &key (imprecision 0.0)
			      exclude-keywords
			      precise-keywords
			      object-name)
  (cond
    ((typep object 'number)
     (if (> imprecision 0.0)
	 (add-imprecision object imprecision :object-name object-name)
	 object))
    ((or (typep object 'symbol) (typep object 'function))
     object)
    ((typep object 'list)
     (deepcopy-list object
		    :imprecision imprecision
		    :exclude-keywords exclude-keywords
		    :precise-keywords precise-keywords))
    ((typep object 'hash-table)
     (deepcopy-hash-table object
			  :imprecision imprecision
			  :exclude-keywords exclude-keywords
			  :precise-keywords precise-keywords))
    ((typep object 'string)
     (copy-seq object))
    ((typep object 'standard-object) 
     (deepcopy-object object
		      :imprecision imprecision
		      :exclude-keywords exclude-keywords
		      :precise-keywords precise-keywords))))


;; This macro is basically just a wrapper for the (original) function,
;; so that i can mix keyword arguments and an arbitrary number of
;; ensuing graph elements ... 
(defmacro graph (name (&key
		       (perma nil) ;; what's this ???
		       (combine-mode ''append)
		       (affect-transition nil)
		       (combine-filter #'all-p)
		       (update-clones t) ;; what's this ???
		       (rand 0))
		 &body graphdata)
  `(funcall #'(lambda () (let ((new-graph (make-instance 'graph)))		      
		      (setf (graph-id new-graph) ,name)    
		      (mapc #'(lambda (obj)
				(cond ((typep obj 'edge) (insert-edge new-graph obj))
				      ((typep obj 'node) (insert-node new-graph obj))))
			    (list ,@graphdata))		      
		      ;; add random blind edges ...
		      (if (> ,rand 0) (randomize-edges new-graph ,rand))  
		      (if (gethash ,name *processor-directory*)
			  ;; update existing instance
			  (let ((cur-instance (gethash ,name *processor-directory*)))
			    (setf (source-graph cur-instance) new-graph)
			    (setf (affect-transition cur-instance) ,affect-transition)
			    (setf (combine-mode cur-instance) ,combine-mode)
			    (setf (combine-filter cur-instance) ,combine-filter)
			    (setf (update-clones cur-instance) ,update-clones)
			    (setf (copy-events cur-instance) (not ,perma))
			    (when ,update-clones
			      (mapc #'(lambda (proc-id)
					(let ((my-clone
					       (gethash proc-id *processor-directory*)))
					  (setf (source-graph my-clone)
						(deepcopy new-graph))
					  (setf (affect-transition my-clone) ,affect-transition)
					  (setf (combine-mode my-clone) ,combine-mode)
					  (setf (combine-filter my-clone) ,combine-filter)
					  (setf (update-clones my-clone) ,update-clones)
					  (setf (copy-events my-clone) (not ,perma))))
				    (clones cur-instance)))
			    cur-instance)			    
			  (setf (gethash ,name *processor-directory*)
				(make-instance 'graph-event-processor :name ,name
					       :graph new-graph :copy-events (not ,perma)
					       :current-node 1 :combine-mode ,combine-mode
					       :affect-transition ,affect-transition
					       :combine-filter ,combine-filter
					       :update-clones ,update-clones)))))))

;;  shorthand for graph
(setf (macro-function 'g) (macro-function 'graph))

;; replace the content (or parts of the content) of a graph ...
(defun graph-add (name new-content)
  (let ((current-graph (source-graph (gethash name *processor-directory*))))
    (mapc #'(lambda (obj)
	      (cond ((typep obj 'edge) (insert-edge current-graph obj))
		    ((typep obj 'node) (insert-node current-graph obj))))
	  new-content)
    (setf (source-graph (gethash name *processor-directory*)) current-graph)))

(defmethod randomize-edges ((g graph) chance )
  (loop for src being the hash-keys of (graph-nodes g)
     do (loop for dest being the hash-keys of (graph-nodes g)
	   do (let ((randval (random 100)))
		(if (and (< randval chance)
			 (not
			  (get-edge g (if (typep src 'sequence)
					  src
					  (list src))
				    dest)))
		    (insert-edge g (edge src dest :prob 0)))))))

;; only for single values (pitch, duration, level etc )
(defmacro values->graph (name event-type values
			 &key (type 'loop)
			   (combine-mode 'append)
			   (affect-transition nil)
			   (randomize 0))
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
			   ;; add random blind edges 
			   (if (> ,randomize 0) (randomize-edges new-graph ,randomize ))
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
				      &key (type 'loop)
					(randomize 0)
					(combine-mode 'append)
					(affect-transition nil))
  `(funcall #'(lambda ()
		(let ((new-graph (make-instance 'graph))
		      (count 1)
		      (len (list-length ,values)))		      
		  (setf (graph-id new-graph) ,name)
		  (mapc #'(lambda (value transdur)	      
			    (insert-node new-graph (node count (,event-type value)))
			    (if (< count len)
				(insert-edge new-graph (edge count (+ count 1)
							     :prob 100 :dur transdur)))
			    (incf count)
			    ) ,values ,transitions)
		  ;; reverse last step
		  (decf count)
		  (if (eq ',type 'loop)
		      (insert-edge new-graph (edge count 1
						   :prob 100 :dur (car (reverse ,transitions)))))
		  ;; add random blind edges ...
		  (if (> ,randomize 0) (randomize-edges new-graph ,randomize))  
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
	      (insert-node new-graph (node count
					   (mid (car note) :lvl level :dur (- (cadr note) 10))))
	      (if (< count len)
		  (insert-edge new-graph (edge count (+ count 1) :prob 100 :dur (cadr note))))
	      (incf count)) notes)
    ;; reverse last step
    (decf count)
    (if (eq type 'loop)
	(insert-edge new-graph (edge count 1 :prob 100 :dur (cadr (car (reverse notes))))))
    ;; add random blind edges ...
    (if (> randomize 0) (randomize-edges new-graph randomize))
    (if (gethash name *processor-directory*)
	(setf (source-graph (gethash name *processor-directory*)) new-graph)
	(setf (gethash name *processor-directory*)
	      (make-instance 'graph-event-processor :name name
			     :graph new-graph :copy-events t
			     :current-node 1 :combine-mode 'append
			     :combine-filter #'all-p)))))


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

(defun clear ()
  ;; first of all stop all events already passed to incudine ...
  (incudine::flush-pending)
  (setf *processor-directory* (make-hash-table :test 'eql))
  (loop for chain being the hash-values of *chain-directory*
     do (deactivate chain))       
  (loop for branch being the hash-values of *branch-directory*
     do (mapc #'deactivate branch))       
  (setf *chain-directory* (make-hash-table :test 'eql))
  (setf *group-directory* (make-hash-table :test 'eql))
  (setf *branch-directory* (make-hash-table :test 'eql))
  (setf *current-group* 'DEFAULT))

(defun merg (chain-or-group-id)
  (if (gethash chain-or-group-id *group-directory*)
      (mapc #'merg (gethash chain-or-group-id *group-directory*))  
      (progn
	(mapc #'deactivate (gethash chain-or-group-id *branch-directory*))
	(setf (gethash chain-or-group-id *branch-directory*) nil))))

(defun dq (chain-id)
  (let* ((branches (gethash chain-id *branch-directory*))
	 (last (car (reverse branches))))
    (deactivate last)
    (setf (gethash chain-id *branch-directory*) (delete last branches))))

(defun stop (&rest chains)  
  (if (<= (length chains) 0)
      (loop for chain being the hash-values of *chain-directory*
	 do (deactivate chain))
      (mapc #'(lambda (id)
		;; if it's a group, stop the group
		(if (gethash id *group-directory*)
		    (mapc #'(lambda (chain)
			      (deactivate (gethash chain *chain-directory*)))
			  (gethash id *group-directory*))
		    ;; if it's a chain, stop the chain ...
		    (deactivate (gethash id *chain-directory*))))
	    chains)))

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

(defmethod update-graph-name ((g graph) new-name &key)
  (setf (graph-id g) new-name)
  (loop for n being the hash-values of (graph-nodes g)
     do (progn (setf (node-global-id n) (list new-name (node-id n)))
	       (loop for ev in (node-content n)
		    do (setf (nth 0 (event-source ev)) new-name)))))

(defun clone (original-id clone-id &key (variance 0.0) (track t) (store t))
  (let ((original (gethash original-id *processor-directory*)))
    (when original
      (let ((clone (deepcopy original :imprecision variance)))
	(when (typep original 'graph-event-processor)
	  (update-graph-name (source-graph clone) clone-id))	
	(setf (name clone) clone-id)
	(setf (chain-bound clone) nil)	
	(when store
	  (setf (gethash clone-id *processor-directory*) clone))
	(when track
	  (unless (member clone-id (clones original))
	    (setf (clones original) (append (clones original) (list clone-id)))))
	clone))))

(defmacro sync-progn (ch &body funcs)
  `(funcall #'(lambda ()
		(let ((chain (gethash ,ch *chain-directory*)))
		  (when chain		    
		    (setf (synced-progns chain)
			  (append (synced-progns chain)
				  (list (lambda () ,@funcs)))))))))

;; set the default group
(defun group (groupname)
  (setf *current-group* groupname))
