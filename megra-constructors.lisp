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

