(in-package :megra)

;; functions and macros to construct event processors ... 

;; only for single values (pitch, duration, level etc)
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
						                      :graph new-graph 
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
					                          :graph new-graph
					                          :current-node 1 :combine-mode ,combine-mode
					                          :affect-transition ,affect-transition
					                          :combine-filter #'all-p)))))))

;; takes notes in the format '(pitch duration) ant turns them into a loop graph
;; which might be randomized
(defun notes->midi-graph (name &key notes (level 0.5) (type 'loop) (randomize 0) (dur *global-default-duration*))
  (let ((new-graph (make-instance 'graph))
	(count 1)
	(len (list-length notes)))		      
    (setf (graph-id new-graph) name)
    (mapc #'(lambda (note)	      
	      (insert-node new-graph (node count
					   (mid (car note)
						:lvl level
						:dur (- (cadr note) 10))))
	      (if (< count len)
		  (insert-edge new-graph (edge count (+ count 1)
					       :prob 100
					       :dur (cadr note))))
	      (incf count)) notes)
    ;; reverse last step
    (decf count)
    (if (eq type 'loop)
	(insert-edge new-graph (edge count 1 :prob 100
				             :dur (cadr (car (reverse notes))))))
    ;; add random blind edges ...
    (if (> randomize 0) (randomize-edges new-graph randomize))
    (if (gethash name *processor-directory*)
	(setf (source-graph (gethash name *processor-directory*)) new-graph)
	(setf (gethash name *processor-directory*)
	      (make-instance 'graph-event-processor :name name
			                            :graph new-graph 
			                            :current-node 1 :combine-mode 'append
			                            :combine-filter #'all-p)))))

;; nucleus, one node, with one repeating edge ... 
(defun nuc (name event &key (overlap 0) (dur *global-default-duration*) (reset t))
  (define-filter name)
  (let* ((graph-proc (if (gethash name *processor-directory*)
			 (gethash name *processor-directory*)
			 (make-instance 'graph-event-processor :name name
					                       :graph nil 
					                       :current-node 1 :combine-mode 'append
					                       :combine-filter #'all-p
					                       :affect-transition nil)))
	 (src-graph (cond ((or reset (not (source-graph graph-proc)))
			   (make-instance 'graph))
			  (t (source-graph graph-proc)))))   
    (when (or reset (not (source-graph graph-proc))) ;; either it's new or reset ...
      (setf (graph-id src-graph) name)   
      (graph-add-direct src-graph (list 
				   (make-instance 'node
						  :id 1 
						  :content (if (typep event 'list)
							       event
							       (list event))
						  :color 'white)))
      (let ((dur (cond ((typep dur 'param-mod-object) dur)
		       (dur (- dur (* dur overlap)))
		       (t (if (typep (event-duration event) 'param-mod-object)
			      (event-duration event)
			      (- (event-duration event)
				 (* (event-duration event) overlap)))))))
	(graph-add-direct src-graph (list (edge 1 1 :dur dur :prob 100)))
	(setf (current-node graph-proc) 1)
	(setf (traced-path graph-proc) '(1))))   
    (setf (source-graph graph-proc) src-graph)    
    (setf (gethash name *processor-directory*) graph-proc)))

;; a cycle ... 
(defun cyc (name events  &key (dur *global-default-duration*) (overlap 0) (rnd 0) (rep 0) (max-rep 4) (reset t) (cmode 'auto))
  (define-filter name)
  (let* ((real-events (if (typep events 'string)
			  (string->cycle-list events)
			  events))
	 (graph-proc (if (gethash name *processor-directory*)
			 (gethash name *processor-directory*)
			 (make-instance 'graph-event-processor :name name
					                       :graph nil 
					                       :current-node 1 :combine-mode cmode
					                       :combine-filter #'all-p
					                       :affect-transition nil)))
	 (src-graph (cond ((or reset (not (source-graph graph-proc)))
			   (make-instance 'graph))
			  (t (source-graph graph-proc))))
	 (dur (cond ((typep dur 'param-mod-object) dur)
		    (dur (- dur (* dur overlap)))))
	 (count 1)
         (last-dur dur))
    (setf (graph-id src-graph) name)
    (loop for (a b) on real-events while b
          do (cond
	       ((and (or (typep a 'event) (typep a 'list)) (or (typep b 'event) (typep b 'list)))
                (insert-node src-graph (node count a))
                (insert-node src-graph (node (+ count 1) b))
                (insert-edge src-graph (edge count (+ count 1) :prob 100 :dur dur))
                (when (> rep 0)
		  (when (< (random 100) rep)
		    (insert-edge src-graph (edge count count :prob 100 :dur dur))
		    (when max-rep
		      (insert-edge src-graph (edge (make-list max-rep :initial-element count) (+ count 1) :dur dur :prob 100)))))
                (incf count))               	        
	       ((and (or (typep a 'event) (typep a 'list)) (typep b 'number))
                (insert-node src-graph (node count a))
                (setf last-dur b))
	       ((and (typep a 'number) (or (typep b 'event) (typep b 'list)))
                (insert-node src-graph (node (+ count 1) b))
                (insert-edge src-graph (edge count (+ count 1) :prob 100 :dur last-dur))
                (when (> rep 0)
		  (when (< (random 100) rep)
		    (insert-edge src-graph (edge count count :prob 100 :dur last-dur))
		    (when max-rep
                      (insert-edge src-graph (edge (make-list max-rep :initial-element count) (+ count 1) :dur last-dur :prob 100)))))
                (incf count))))
    (if (typep (car (last real-events)) 'number)
        (insert-edge src-graph (edge count 1 :prob 100 :dur (car (last real-events))))
        (insert-edge src-graph (edge count 1 :prob 100 :dur dur)))          
    (when (or (not (source-graph graph-proc))
              (and (source-graph graph-proc) (not (gethash (current-node graph-proc) (graph-nodes src-graph)))))
      ;; either it's new or reset ...      
      (setf (current-node graph-proc) 1)
      (setf (traced-path graph-proc) '(1)))    
    (rebalance-edges src-graph)
             ;; randomize if necessary ... 
    (if (> rnd 0) (randomize-edges src-graph rnd dur))
    (setf (source-graph graph-proc) src-graph)
    (setf (gethash name *processor-directory*) graph-proc)))

;; brownian pan on something
(defmacro bpan (&body selector)
  `(for ,@selector (always (pos (brownian -1.0 1.0)))))

;; star
;; tstar
;; bstar
