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
    (cond ((< newval min-res) min-res)
	  ((> newval max-res) max-res)
	  (t newval))))

(defun add-pitch-imprecision (pitch imprecision)
  (cm::note (add-imprecision (cm::hertz pitch) imprecision) :hz))

(defun is-note-name (symbol)
  (when (typep symbol 'symbol)
    (handler-case		    
        (cm::hertz symbol)
      (simple-error (e) nil))))

(defun deepcopy-list (list &key
			     (imprecision 0.0)
			     exclude-keywords
			     precise-keywords
			     functors)
  (if (not (listp (cdr list))) ;; special dotted pair case
      (cons (deepcopy (car list)) (deepcopy (cdr list)))
      (remove nil ;; in case an element wasn't copied ...
	      (mapcar #'(lambda (thing)	      
		          (deepcopy thing
				    :imprecision imprecision
				    :exclude-keywords exclude-keywords
				    :precise-keywords precise-keywords
				    :functors functors))
                      list))))

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


(defmethod deepcopy-query-result ((q vom::query-result)  &key (imprecision 0.0)
				                              exclude-keywords
				                              precise-keywords
				                              functors)
  (vom::make-query-result :last-state (vom::query-result-last-state q)
                          :current-state (vom::query-result-current-state q)
                          :symbol (vom::query-result-symbol q)))

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

(defmethod deepcopy-object ((tr transition-event)
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

(defmethod deepcopy-object ((e event) &key (imprecision 0.0)
					exclude-keywords
					precise-keywords
					functors)
  (deepcopy-generic-object e
			   :imprecision imprecision
			   :exclude-keywords (append
					      exclude-keywords
					      '(source))
			   :precise-keywords precise-keywords
			   :functors functors))

(defmethod deepcopy-object ((e event-processor-wrapper) &key (imprecision 0.0)
							  exclude-keywords
							  precise-keywords
							  functors)
  (let ((clone (deepcopy-generic-object e
					:imprecision imprecision
					:exclude-keywords (append
							   exclude-keywords
							   '(successor predecessor))
					:precise-keywords precise-keywords
					:functors functors))
	(proc-name (name (wrapper-wrapped-processor e))))
    ;; give copied wrapped processor a new name 
    (setf (name (wrapper-wrapped-processor clone))
	  (intern (concatenate
		   'string
		   (symbol-name proc-name)
		   "-"
		   (symbol-name (gensym)))))    
    ;; store reference in global processor directory
    (setf (gethash (name (wrapper-wrapped-processor clone)) *processor-directory*)
	  (wrapper-wrapped-processor clone))
    clone))

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
    ((typep object 'symbol)
     (if (is-note-name object)
	 (add-pitch-imprecision object imprecision)
	 object))
    ((typep object 'function) object)    
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
    ((typep object 'vom::query-result) 
     (deepcopy-query-result object
		            :imprecision imprecision
		            :exclude-keywords exclude-keywords
		            :precise-keywords precise-keywords
		            :functors functors))
    ((typep object 'standard-object) 
     (deepcopy-object object
		      :imprecision imprecision
		      :exclude-keywords exclude-keywords
		      :precise-keywords precise-keywords
		      :functors functors))))
