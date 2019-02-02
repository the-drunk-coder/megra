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
			     :graph new-graph :copy-events t
			     :current-node 1 :combine-mode 'append
			     :combine-filter #'all-p)))))

;; nucleus, one node, with one repeating edge ... 
(defun nuc (name event &key (overlap 0) (gap 400) (reset t))  
  (let* ((graph-proc (if (gethash name *processor-directory*)
			 (gethash name *processor-directory*)
			 (make-instance 'graph-event-processor :name name
					:graph nil :copy-events t
					:current-node 1 :combine-mode 'zip
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
      (let ((dur (cond ((typep gap 'param-mod-object) gap)
		       (gap (- gap (* gap overlap)))
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
(defun cyc (name events  &key (gap 400) (overlap 0) (rnd 0) (rep 0) (max-rep 4) (reset t))
  (let* ((graph-proc (if (gethash name *processor-directory*)
			 (gethash name *processor-directory*)
			 (make-instance 'graph-event-processor :name name
					:graph nil :copy-events t
					:current-node 1 :combine-mode 'zip
					:combine-filter #'all-p
					:affect-transition nil)))
	 (src-graph (cond ((or reset (not (source-graph graph-proc)))
			   (make-instance 'graph))
			  (t (source-graph graph-proc))))
	 (dur (cond ((typep gap 'param-mod-object) gap)
		    (gap (- gap (* gap overlap)))))
	 (count 1))
    (setf (graph-id src-graph) name)
    (loop for (a b) on events
       do (let ((duration (if (and b (typep b 'integer))
			      b gap)))
	    (unless (typep a 'number)
	      (insert-node src-graph (node count a))
	      (when (or reset (not (source-graph graph-proc)))
		;; either it's new or reset ... 		
		(when b
		  (insert-edge src-graph
			       (edge count (+ count 1) :prob 100 :dur duration)))
		(when (> rep 0)
		  (when (< (random 100) rep)
		    (insert-edge src-graph (edge count count :prob 100 :dur duration))
		    (when max-rep
		      (insert-edge src-graph
				   (edge (make-list max-rep :initial-element count)
					 (if b
					     (+ count 1)
					     1)
					 :dur duration :prob 100))))))
	      (incf count))))       
    (when (or reset (not (source-graph graph-proc))) ;; either it's new or reset ...
      (insert-edge src-graph (edge (- count 1) 1 :prob 100 :dur dur))
      (setf (current-node graph-proc) 1)
      (setf (traced-path graph-proc) '(1)))
    (rebalance-edges src-graph)
    ;; randomize if necessary ... 
    (if (> rnd 0) (randomize-edges src-graph rnd dur))
    (setf (source-graph graph-proc) src-graph)    
    (setf (gethash name *processor-directory*) graph-proc)))



;; star
;; tstar
;; bstar
