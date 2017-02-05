;; structural
(defun node (id &rest content)
  (make-instance 'node :id id :content content))

(defun edge (src dest &key prob dur)
  (make-instance 'edge :src src :dest dest :prob prob :content `(,(make-instance 'transition :dur dur))))

;; this macro is basically just a wrapper for the (original) function,
;; so that i can mix keyword arguments and an arbitrary number of
;; ensuing graph elements ... 
(defmacro graph (name (&key (perma nil)) &body graphdata)
  `(funcall #'(lambda () (let ((new-graph (make-instance 'graph)))		      
		      (setf (graph-id new-graph) ,name)    
		      (mapc #'(lambda (obj)
				(cond ((typep obj 'edge) (insert-edge new-graph obj))
				      ((typep obj 'node) (insert-node new-graph obj))))
			    (list ,@graphdata))
		      (if (gethash ,name *processor-directory*)
			  (setf (source-graph (gethash ,name *processor-directory*)) new-graph)
			  (setf (gethash ,name *processor-directory*)
				(make-instance 'graph-event-processor
					       :graph new-graph :copy-events (not ,perma)
					       :current-node 1))))
		 ,name)))

;; dispatching ... one dispatcher per active event processor ...
(defun dispatch (&rest event-processors)
  (labels
      ((connect (processors)
	 (when (cadr processors)
	   (setf (successor (gethash (car processors) *processor-directory* ))
		 (gethash (cadr processors) *processor-directory*))
	   (connect (cdr processors)))))
    (connect event-processors))
  ;; if the first event-processor is not active yet, create a dispatcher to dispatch it ... 
  (unless (is-active (gethash (car event-processors) *processor-directory*))
    ;;(princ "new dispatcher")
    (let ((dispatcher (make-instance 'event-dispatcher)))
      (activate (car event-processors))      
      (perform-dispatch dispatcher (car event-processors) (incudine:now))))) 

;; modifying ... always check if the modifier is already present !
(defun brownian-motion (name param &key step wrap limit ubound lbound (track-state t))
  (let ((new-inst (make-instance 'brownian-motion :step step :mod-prop param
				 :upper-boundary ubound
				 :lower-boundary lbound
				 :is-bounded limit
				 :is-wrapped wrap
				 :track-state track-state)))
    (when (gethash name *processor-directory*)
      (setf (is-active new-inst) t))
    (setf (gethash name *processor-directory*) new-inst))
  name)

(defun oscillate-between (name param upper-boundary lower-boundary &key cycle type (track-state t))
  (let ((new-inst (make-instance 'oscillate-between :mod-prop param
				 :cycle cycle
				 :upper-boundary upper-boundary
				 :lower-boundary lower-boundary
				 :track-state track-state)))
    (when (gethash name *processor-directory*)
      (setf (is-active new-inst) t))
    (setf (gethash name *processor-directory*) new-inst))
  name)

;; events
(defun string-event (msg)
  (make-instance 'string-event :msg msg))

(defun mid (pitch &key dur lvl)
  (make-instance 'midi-event :pitch pitch :lvl lvl :dur dur))

(defun grain (folder file &key  (dur 256)
			    (lvl 0.5)
			    (pos 0.5)
			    (start 0.0)
			    (rate 1.0)
			    (hp-freq 10)
			    (hp-q 1)
			    (pf-freq 1000)
			    (pf-q 10)
			    (pf-gain 0.0)
			    (lp-freq 19000)
			    (lp-q 1)
			    (lp-dist 0.0)
			    (atk 7)
			    (rel 7)
			    )
  (make-instance 'grain-event :lvl lvl :dur dur :start start :pos pos :hp-freq hp-freq
		 :rate rate :hp-freq hp-freq :hp-q hp-q
		 :pf-freq pf-freq :pf-q pf-q :pf-gain pf-gain
		 :lp-freq lp-freq :lp-q lp-q :lp-dist lp-dist
		 :atk atk :rel rel :sample-folder folder :sample-file file))

;; deactivate ... if it's a modifying event processor, delete it ... 
(defun deactivate (event-processor-id)
  (setf (is-active (gethash event-processor-id *processor-directory*)) nil)
  ;; this is as un-functional as it gets, but anyway ...
  (if (typep (gethash 'tres-br *processor-directory*) 'modifying-event-processor)
      (setf (gethash event-processor-id *processor-directory*) nil)))


(defun activate (event-processor-id)
  (setf (is-active (gethash event-processor-id *processor-directory*)) t))

