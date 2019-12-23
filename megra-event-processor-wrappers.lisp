(in-package :megra)

(defclass event-processor-wrapper (event-processor)
  ((act :accessor wrapper-act :initarg :act :initform t)
   (wrapped-processor :accessor wrapper-wrapped-processor
		      :initarg :wrapped-processor)))

(defmethod push-tmod ((w event-processor-wrapper) tmod &key)
  (push-tmod (wrapper-wrapped-processor w) tmod))

(defmethod pop-tmod ((w event-processor-wrapper) &key)
  (pop-tmod (wrapper-wrapped-processor w)))

(defmethod pull-events ((w event-processor-wrapper) &key)
  (let ((ev (if (successor w)
		(apply-self w (pull-events (successor w)))
		(current-events w))))
    (when (wrapper-act w) (post-processing w))
    ev))

;; pass through apply-self ?
(defmethod current-events ((w event-processor-wrapper) &key)
  (pull-events (wrapper-wrapped-processor w)))

(defmethod current-transition ((w event-processor-wrapper) &key)
  (pull-transition (wrapper-wrapped-processor w)))

(defmethod combine-filter ((w event-processor-wrapper))
  (combine-filter (wrapper-wrapped-processor w)))


;;;;;;;;;;;;;;;; GENERIC Population Control ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; population here refers to the number of nodes in a graph ...

(defclass generic-population-control ()
  ((variance :accessor population-control-var :initarg :variance)
   (method :accessor population-control-method :initarg :method)
   (durs :accessor population-control-durs :initarg :durs)
   (phoe :accessor population-control-higher-order-probability :initarg :phoe)
   (hoe-max :accessor population-control-higher-order-max-order :initarg :hoe-max)
   (exclude :accessor population-control-exclude :initarg :exclude)))

;;;;;;;;;;;;;;;; Simple Probablistic Population Control ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; grow and prune the graph with a certain probability

(defclass probability-population-control (event-processor-wrapper
					  generic-population-control)
  ((pgrowth :accessor population-control-pgrowth :initarg :pgrowth)
   (pprune :accessor population-control-pprune :initarg :pprune)))

(defun find-keyword-val (keyword seq &key default)
  (if (and
       (member keyword seq)
       (> (length (member keyword seq)) 0) ;; check if there's chance the keyword has a value ...
       (not (eql (type-of (cadr (member keyword seq))) 'keyword)))
      (let* ((pos (position keyword seq))
	     (val (nth (+ pos 1) seq)))
	val)
      default))

(defun probctrl (act pgrowth pprune
		 &rest rest)
  (let ((method (find-keyword-val :method rest :default 'triloop))
	(variance (find-keyword-val :var rest :default 0.2))
	(durs (find-keyword-val :durs rest :default nil))
	(hoe-max (find-keyword-val :hoe-max rest :default 4))
	(hoe (find-keyword-val :hoe rest :default 4))
	(exclude (find-keyword-val :exclude rest :default nil))
	(wrapped-processor (if (typep (last rest) 'symbol)
			       (gethash (last rest) *processor-directory*)
			       (car (last rest)))))
    (make-instance 'probability-population-control
		   :wrapped-processor wrapped-processor
		   :act act
		   :name (gensym)		   		   
		   :variance variance
		   :pgrowth pgrowth
		   :pprune pprune
		   :method method
		   :durs durs
		   :phoe hoe
		   :hoe-max hoe-max
		   :exclude exclude)))

(defmethod post-processing((g probability-population-control) &key)
  (when (< (random 100) (population-control-pgrowth g))
    (let ((order (if (< (random 100)
			(population-control-higher-order-probability g))
		     (+ 2 (random
			   (- (population-control-higher-order-max-order g) 2)))
		     nil)))
      (grow (wrapper-wrapped-processor g)
	    :var (population-control-var g)
	    :durs (population-control-durs g)
	    :method (population-control-method g)
	    :higher-order order)))
  (when (< (random 100) (population-control-pprune g))
    (prune-graph
     (wrapper-wrapped-processor g)       
     :exclude (population-control-exclude g))))


;;;;;;;;;;;;;;;; Simple Artifical Life Model ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; growing an eventprocessor using a resource model ... cold also be named
;; free energy model or something like that ... 

(defclass lifemodel-control (event-processor-wrapper generic-population-control)
  ((growth-cycle :accessor lmc-growth-cycle :initarg :growth-cycle)
   (lifecycle-count :accessor lmc-lifecycle-count :initform 0)
   (apoptosis :accessor lmc-apoptosis :initarg :apoptosis :initform t)
   (node-lifespan :accessor lmc-node-lifespan :initarg :node-lifespan :initform *average-node-lifespan*)
   (node-lifespan-var :accessor lmc-node-lifespan-var :initarg :node-lifespan-var :initform *node-lifespan-variance*)
   (autophagia :accessor lmc-autophagia :initarg :autophagia :initform t)
   (local-resources :accessor lmc-local-resources :initarg :local-resources :initform *default-local-resources*)
   (local-cost :accessor lmc-local-cost :initarg :local-cost-modifier :initform *growth-cost*)
   (local-apoptosis-regain :accessor lmc-local-apoptosis-regain :initarg :apoptosis-regain :initform *apoptosis-regain*)
   (local-autophagia-regain :accessor lmc-local-autophagia-regain :initarg :autophagia-regain :initform *autophage-regain*)))

(defmethod post-processing ((l lifemodel-control) &key)  
  (incf (lmc-lifecycle-count l))  
  ;; growth point reached
  (let* ((src (graph-id (source-graph (wrapper-wrapped-processor l))))
	 (cur-node-id (current-node (wrapper-wrapped-processor l)))
	 (cur-node (gethash cur-node-id (graph-nodes (source-graph (wrapper-wrapped-processor l)))))
	 (eaten-node 0)) ;; node "eaten" by autophagia ...	     
    ;; growth or no growth ?
    (when (>= (lmc-lifecycle-count l) (lmc-growth-cycle l))
      (setf (lmc-lifecycle-count l) 0) ;; reset growth cycle
      (if (> (+ (lmc-local-resources l) *global-resources*) (lmc-local-cost l))
	  ;; first case: enough resoures available
	  (let ((order (if (< (random 100)
			      (population-control-higher-order-probability l))
			   (+ 2 (random
				 (- (population-control-higher-order-max-order l) 2)))
			   nil)))
	    ;; grow graph 
	    (grow (wrapper-wrapped-processor l)
		  :var (population-control-var l)
		  :durs (population-control-durs l)
		  :method (population-control-method l)
		  :higher-order order)	    
	    ;; decrease resources
	    (if (>= (lmc-local-resources l) (lmc-local-cost l))
		(setf (lmc-local-resources l) (- (lmc-local-resources l)
						 (lmc-local-cost l)))
		(if (>= *global-resources* (lmc-local-cost l))
		    (setf *global-resources* (- *global-resources* (lmc-local-cost l)))
		    ;; otherwise, split ...
		    (let ((tmp-cost (lmc-local-cost l)))
		      (setf tmp-cost (- tmp-cost (lmc-local-resources l)))
		      (setf (lmc-local-resources l) 0.0)
		      (setf *global-resources* (- *global-resources* tmp-cost)))))
	    (incudine::msg info "GROW at ~D - local: ~D global: ~D"
			   src
			   (lmc-local-resources l)
			   *global-resources*))
	  ;; else: autophagia if specified ...
	  (when (and (lmc-autophagia l)
		     (> (graph-size (source-graph (wrapper-wrapped-processor l))) 1))	    
	    ;; send prune/shrink
	    (let ((rnd-node (random-node-id (source-graph (wrapper-wrapped-processor l)) nil)))
	      (prune-graph (wrapper-wrapped-processor l) :node-id cur-node-id)
	      (setf eaten-node rnd-node))	    
	    ;; add regain to local	    
	    (setf (lmc-local-resources l)
		  (+ (lmc-local-resources l)
		     (lmc-local-autophagia-regain l)))	    
	    (incudine::msg info "AUTO at ~D - local: ~D global: ~D - ~D is starving!"
			   src
			   (lmc-local-resources l)
			   *global-resources*
			   src))))
    ;; handle apoptosis:
    ;; check if current node is old enough (regarding eventual variance)
    ;; delete if old
    ;; add regain ... 
    (when (and
	   (lmc-apoptosis l)
	   (or (and *dont-let-die* (> (graph-size (source-graph (wrapper-wrapped-processor l))) 1))
	       (not *dont-let-die*))	       
	   (> (node-age cur-node)
	      (add-var (lmc-node-lifespan l)
		       (lmc-node-lifespan-var l))))
      (unless (eql cur-node-id eaten-node)
	(prune-graph (wrapper-wrapped-processor l) :node-id cur-node-id)    
	(setf (lmc-local-resources l)
	      (+ (lmc-local-resources l)
		 (lmc-local-apoptosis-regain l))))      
      (incudine::msg info
		     "APOP at ~D - local: ~D global: ~D - node ~D your time has come !"
		     src		     		     
		     (lmc-local-resources l)
		     *global-resources*
		     cur-node-id))))

(defun add-var (orig var)
  (floor (+ orig (* (* (- 20000 (random 40000)) var)
	            (/ orig 20000)))))

;; lifemodel works more in minimalistic contexts rather than algorave,
;; i suppose ...
(defun inner-lifemodel (act growth-cycle lifespan rest)
  (let ((method (find-keyword-val :method rest :default 'triloop))
	(variance (find-keyword-val :var rest :default 0.2))
	(autophagia (find-keyword-val :autophagia rest :default t))
	(apoptosis (find-keyword-val :apoptosis rest :default t))        
	(durs (find-keyword-val :durs rest :default nil))
	(hoe-max (find-keyword-val :hoe-max rest :default 4))
	(hoe (find-keyword-val :hoe rest :default 4))
	(exclude (find-keyword-val :exclude rest :default nil))
	(wrapped-processor (if (typep (last rest) 'symbol)
			       (gethash (last rest) *processor-directory*)
			       (car (last rest)))))
    (make-instance 'lifemodel-control
		   :name (gensym)
		   :act act
		   :wrapped-processor wrapped-processor
		   :growth-cycle growth-cycle		 
		   :variance variance		 
		   :method method
		   :durs durs
		   :phoe hoe
		   :node-lifespan lifespan
		   :hoe-max hoe-max
		   :exclude exclude
		   :autophagia autophagia
		   :apoptosis apoptosis)))

(defun lifemodel (act growth-cycle lifespan &rest rest)
  (inner-lifemodel act growth-cycle lifespan rest))

(defun lm (act growth-cycle lifespan &rest rest)
  (inner-lifemodel act growth-cycle lifespan rest))

;; count
(defclass count-wrapper (event-processor-wrapper)
  ((on-count :accessor on-count :initarg :on-count)
   (function :accessor count-control-function :initarg :function)
   (counter :accessor control-counter :initform 0)))

(defmethod post-processing ((c count-wrapper) &key)
  (incf (control-counter c))
  (when (eql (control-counter c) (on-count c))
    (funcall (count-control-function c) (wrapper-wrapped-processor c))
    (setf (control-counter c) 0)))

(defun evr (act count fun proc)
  (make-instance 'count-wrapper
                 :act act
                 :name (gensym)
                 :on-count count 
                 :function fun
                 :wrapped-processor proc))

;; prob
(defclass prob-wrapper (event-processor-wrapper)
  ((prob :accessor prob-wrapper-prob :initarg :prob)
   (function :accessor prob-control-function :initarg :function)))

(defmethod post-processing ((p prob-wrapper) &key)
  (when (< (random 100) (prob-wrapper-prob p))
    (funcall (prob-control-function p) (wrapper-wrapped-processor p))))

(defun pprob (act prob fun proc) 
  (make-instance 'prob-wrapper
                 :act act
                 :name (gensym)
                 :prob prob 
                 :function fun                
                 :wrapped-processor proc))

(defclass duplicator (event-processor-wrapper)
  ((duplicates :accessor duplicates :initarg :duplicates)))

(defmethod post-processing ((d duplicator) &key)
  (loop for dup in (duplicates d)
        do (pull-transition dup)))

(defmethod pull-events ((w duplicator) &key)
  (if (not (wrapper-act w))
      (let ((ev (if (successor w)
		    (apply-self (wrapper-wrapped-processor w)
			        (pull-events (successor w)))
		    (current-events (wrapper-wrapped-processor w)))))
        (when (wrapper-act w) (post-processing w))
        ev)
      (let ((ev (if (successor w)
		    (nconc (apply-self (wrapper-wrapped-processor w)
 			               (pull-events (successor w)))
                           (loop for dup in (duplicates w)
                                 collect (pull-events dup)))
		    (nconc (current-events (wrapper-wrapped-processor w))
                           (loop for dup in (duplicates w)
                                 collect (pull-events dup))))))
        (when (wrapper-act w) (post-processing w))
        ;; this can be assembled more elegantly, i guess ... 
        (alexandria::flatten ev))))

(defun dup (act num proc)
  (let ((duplicates
          (loop for p from 1 to num
                collect (deepcopy proc))))    
    (make-instance 'duplicator
                   :act act
                   :name (gensym)
                   :duplicates duplicates
                   :wrapped-processor proc)))

;; dup - duplicate .. (new wrapper)
;; (dup t 3 (cyc ..)
;;     (lm t 20 20 :var 0.2) ;; recursive application necessary ! 
;;     (every t 4 (skip 2))

(defclass applicator (event-processor-wrapper)
  ((events-to-apply :accessor applicator-events :initarg :events)))

(defmethod pull-events ((w applicator) &key)
  (if (successor w)
      (if (wrapper-act w)
          (let ((other-events (current-events w)))
            (loop for aev in (applicator-events w)
                  do (loop for i from 0 to (- (length other-events) 1)
                           do (setf (nth i other-events)
                                    (combine-single-events aev (nth i other-events)))))
            (apply-self-2 w other-events (pull-events (successor w))))
          (apply-self w (pull-events (successor w))))
      (let ((other-events (current-events w)))
        (loop for aev in (applicator-events w)
              do (loop for i from 0 to (- (length other-events) 1)
                       do (setf (nth i other-events)
                                (combine-single-events aev (nth i other-events)))))
        other-events)))

(defun pear (act &rest events-and-proc)
  (make-instance 'applicator
                 :act act
                 :name (gensym)
                 :events (butlast events-and-proc)
                 :wrapped-processor (car (last events-and-proc))))

;; FUNCTIONS 
;; SKIP
(defun skip (num &optional proc)    
  (if proc
      (progn
        (loop for a from 0 to (- num 1)
              do (progn                              
                   (pull-events proc :skip-successor t)
                   (pull-transition proc :skip-successor t)))
        proc)
      (lambda (nproc) (skip num nproc))))

;; GROWN
(defun grown (n var method &optional proc)  
  (if proc
      (progn (loop for a from 0 to (nth 0 args)
                   do (grow proc :var var :method method))
             proc)
      (lambda (nproc) (grown n var method nproc))))

(defun grown2 (n var method &optional proc)  
  (if proc
      (progn (loop for a from 0 to (nth 0 args)
                   do (grow2 proc :var var :method method))
             proc)
      (lambda (nproc) (grown2 n var method nproc))))

;; haste 4 0.5 - apply tempo mod for the next n times (only on base proc)
(defun haste (num mod &optional proc)  
  (if proc
      (progn (loop for a from 0 to (- num 1)
                   do (push-tmod proc mod))
             proc)
      (lambda (nproc) (haste num mod nproc))))

;; relax 4 0.5 - apply tempo mod for the next n times (only on base proc)
(defun relax (num mod &optional proc)  
  (if proc
      (progn (loop for a from 0 to (- num 1)
                   do (push-tmod proc (coerce (/ 1.0 mod) 'float)))
             proc)
      (lambda (nproc) (relax num mod nproc))))


;; rew 3 - rewind (set to state n back in traced path)
;; needs traced path for pfa and state setter method, ideally for both ... 


