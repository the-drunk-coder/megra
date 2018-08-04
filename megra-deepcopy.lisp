(in-package :megra)

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
			     precise-keywords
			     functors)
  (remove nil ;; in case an element wasn't copied ...
	  (mapcar #'(lambda (thing)	      
		      (deepcopy thing
				:imprecision imprecision
				:exclude-keywords exclude-keywords
				:precise-keywords precise-keywords
				:functors functors)) list)))

(defun deepcopy-hash-table (orig &key (imprecision 0.0)
				   exclude-keywords
				   precise-keywords
				   functors)
  (let ((new-table (make-hash-table :test (hash-table-test orig))))
    (loop for key being the hash-keys of orig
       do (setf (gethash key new-table)
		(deepcopy (gethash key orig)
			  :imprecision imprecision
			  :exclude-keywords exclude-keywords
			  :precise-keywords precise-keywords
			  :functors functors)))
    new-table))


(defmethod deepcopy-generic-object (object
				    &key (imprecision 0.0)
				      exclude-keywords
				      precise-keywords
				      functors)
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
		      :object-name (slot-definition-name slot)
		      :functors functors))		    
		    (t (deepcopy
			(slot-value object (slot-definition-name slot))
			:imprecision imprecision
			:exclude-keywords exclude-keywords
			:precise-keywords precise-keywords
			:object-name (slot-definition-name slot)
			:functors functors))))))
    copy))

(defmethod deepcopy-object ((o standard-object)
			    &key (imprecision 0.0)
			      exclude-keywords
			      precise-keywords
			      functors)
  (deepcopy-generic-object o
			   :imprecision imprecision
			   :exclude-keywords exclude-keywords
			   :precise-keywords precise-keywords
			   :functors functors))


(defmethod deepcopy-object ((n node) &key (imprecision 0.0)
				       exclude-keywords
				       precise-keywords
				       functors)
  (deepcopy-generic-object n
			   :imprecision imprecision
			   :exclude-keywords (append
					      exclude-keywords '(global-id id age))
			   :precise-keywords precise-keywords
			   :functors functors))

(defmethod deepcopy-object ((e edge) &key (imprecision 0.0)
				       exclude-keywords
				       precise-keywords
				       functors)
  (deepcopy-generic-object e
			   :imprecision imprecision
			   :exclude-keywords (append exclude-keywords
						     '(source destination))
			   :precise-keywords (append precise-keywords
						     '(probability))
			   :functors functors))

(defmethod deepcopy-object ((tr transition)
			    &key (imprecision 0.0)
			      exclude-keywords
			      precise-keywords
			      functors)
  (deepcopy-generic-object tr
			   :imprecision imprecision
			   :exclude-keywords  exclude-keywords
			   :precise-keywords (append precise-keywords
						     '(dur))
			   :functors functors))

(defmethod deepcopy-object ((g graph) &key (imprecision 0.0)
					exclude-keywords
					precise-keywords
					functors)
  (deepcopy-generic-object g
			   :imprecision imprecision
			   :exclude-keywords (append
					      exclude-keywords
					      '(id
						max-id
						highest-edge-order
						event-source))
			   :precise-keywords precise-keywords
			   :functors functors))

(defmethod deepcopy-object ((e event-processor) &key (imprecision 0.0)
						  exclude-keywords
						  precise-keywords
						  functors)
  (deepcopy-generic-object e
			   :imprecision imprecision
			   :exclude-keywords (append
					      exclude-keywords
					      '(successor predecessor))
			   :precise-keywords precise-keywords
			   :functors functors))

(defmethod deepcopy-object ((g graph-event-processor) &key (imprecision 0.0)
							exclude-keywords
							precise-keywords
							functors)
  (deepcopy-generic-object g
			   :imprecision imprecision
			   :exclude-keywords (append
					      exclude-keywords
					      '(successor predecessor))
			   :precise-keywords (append precise-keywords
						     '(current-node
						       node-steps
						       traced-path
						       trace-length))
			   :functors functors))

(defmethod deepcopy-object ((e shrink-event) &key (imprecision 0.0)
					       exclude-keywords
					       precise-keywords
					       functors)
  (deepcopy-generic-object e
			   :imprecision imprecision
			   :exclude-keywords exclude-keywords
			   :precise-keywords (append precise-keywords
						     '(exclude durs))
			   :functors functors))

(defmethod deepcopy-object ((e growth-event) &key (imprecision 0.0)
					       exclude-keywords
					       precise-keywords
					       functors)
    (let ((chance (random 100)))
      (when (< chance (event-growth-replicate e))
	(deepcopy-generic-object e
				 :imprecision imprecision
				 :exclude-keywords exclude-keywords
				 :precise-keywords (append precise-keywords
							   '(replicate
							     durs
							     variance))
				 :functors functors))))

(defmethod deepcopy-object ((e population-control-event) &key (imprecision 0.0)
							   exclude-keywords
							   precise-keywords
							   functors)      
  (deepcopy-generic-object e
			   :imprecision imprecision
			   :exclude-keywords exclude-keywords
			   :precise-keywords (append precise-keywords
						     '(pgrow
						       pprune
						       exclude
						       durs
						       variance
						       phoedge
						       hoedge-max))
				 :functors functors))

(defmethod deepcopy (object &key (imprecision 0.0)
			      exclude-keywords
			      precise-keywords
			      object-name
			      functors)
  (cond
    ((typep object 'number)
     (let ((temp (if (> imprecision 0.0)
		     (add-imprecision object imprecision :object-name object-name)
		     object)))
       (if functors
	   (loop for functor in functors
	      do (when (member object-name (car functor))
		   (setf temp
			 (funcall (caadr functor) temp (cadadr functor))))))
       temp))
    ((or (typep object 'symbol) (typep object 'function))
     object)
    ((typep object 'list)
     (deepcopy-list object
		    :imprecision imprecision
		    :exclude-keywords exclude-keywords
		    :precise-keywords precise-keywords
		    :functors functors))
    ((typep object 'hash-table)
     (deepcopy-hash-table object
			  :imprecision imprecision
			  :exclude-keywords exclude-keywords
			  :precise-keywords precise-keywords
			  :functors functors))
    ((typep object 'string)
     (copy-seq object))
    ((typep object 'standard-object) 
     (deepcopy-object object
		      :imprecision imprecision
		      :exclude-keywords exclude-keywords
		      :precise-keywords precise-keywords
		      :functors functors))))
