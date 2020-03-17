(in-package :megra)

;;;;;;;;;;;;;;;; Simple Artifical Life Model ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some global parameters ...
(defparameter *global-resources* 35.0) ;; just guessing ...
(defparameter *global-regain* 0.2) ;; just guessing ...
(defparameter *growth-cost* 1.0) ;; cost to grow one node
(defparameter *autophage-regain* 0.7) ;; resource regain when forced
(defparameter *apoptosis-regain* 0.5) ;; generic regain through planned expiration
(defparameter *default-local-resources* 8) ;; again just guessing
(defparameter *average-node-lifespan* 15) ;; a node survives this many steps on average
(defparameter *node-lifespan-variance* 0.1)
(defparameter *dont-let-die* t) ;; always keep at least one node ...  

;; setters for global parameters ...
(defun global-resources (res)
  (setf *global-resources* res))

(defun globres (res)
  (setf *global-resources* res))

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

(defun add-var (orig var)
  (floor (+ orig (* (* (- 20000 (random 40000)) var) (/ orig 20000)))))

(defmethod post-processing ((l lifemodel-control) &key)  
  (incf (lmc-lifecycle-count l))  
  ;; growth point reached
  (let* ((cur-symbol (vom::query-result-symbol (wrapper-wrapped-processor l)))
	 (eaten-node nil)) ;; node "eaten" by autophagia ...	     
    ;; growth or no growth ?
    ;; that is, is the growth period reached ...
    (when (>= (lmc-lifecycle-count l) (lmc-growth-cycle l))
      (setf (lmc-lifecycle-count l) 0) ;; reset growth cycle
      (if (> (+ (lmc-local-resources l) *global-resources*) (lmc-local-cost l))
	  ;; first case: enough resoures available for this generator to grow ... 
	  (progn
	    ;; grow graph 
	    (grow (wrapper-wrapped-processor l)
		  :var (population-control-var l)
		  :durs (population-control-durs l)
		  :method (population-control-method l)
		  :higher-order (if (< (random 100) (population-control-higher-order-probability l))
			            (+ 2 (random (- (population-control-higher-order-max-order l) 2)))
			            0))	    
	    ;; decrease resources
	    (if (>= (lmc-local-resources l) (lmc-local-cost l))
		(setf (lmc-local-resources l) (- (lmc-local-resources l) (lmc-local-cost l)))
		(if (>= *global-resources* (lmc-local-cost l))
		    (setf *global-resources* (- *global-resources* (lmc-local-cost l)))
		    ;; otherwise, split ...
		    (let ((tmp-cost (lmc-local-cost l)))
		      (setf tmp-cost (- tmp-cost (lmc-local-resources l)))
		      (setf (lmc-local-resources l) 0.0)
		      (setf *global-resources* (- *global-resources* tmp-cost))))))
	  ;; else: autophagia if specified ...
	  (when (and (lmc-autophagia l) (> (length (vom::alphabet (inner-generator (wrapper-wrapped-processor l)))) 1))
	    ;; send prune/shrink
	    (let ((rnd-symbol (alexandria::random-elt (vom::alphabet (inner-generator (wrapper-wrapped-processor l))))))
	      (prune (wrapper-wrapped-processor l) :node-id rnd-symbol)
	      (setf eaten-node rnd-symbol))	    
	    ;; add regain to local	    
	    (setf (lmc-local-resources l)
		  (+ (lmc-local-resources l)
		     (lmc-local-autophagia-regain l)))))
    ;; handle apoptosis:    
    (when (and
	   (lmc-apoptosis l) ;; first, check if apoptosis is even specified ...
           ;; then, check if the whole generator can die (run out of symbols) or not ...
	   (or (and *dont-let-die* (> (length (vom::alphabet (inner-generator (wrapper-wrapped-processor l)))) 1)) (not *dont-let-die*))
           ;; then, check if the symbol has ages enough ... 
	   (> (gethash cur-symbol (ages (wrapper-wrapped-processor l))) (add-var (lmc-node-lifespan l) (lmc-node-lifespan-var l))))
      ;; unless the current symbol has randomly been eaten before, remove it ...
      (unless (eql cur-symbol eaten-node)
	(prune (wrapper-wrapped-processor l) :node-id cur-symbol)
        ;; add gained resources back ... 
	(setf (lmc-local-resources l) (+ (lmc-local-resources l) (lmc-local-apoptosis-regain l))))))))



;; lifemodel works more in minimalistic contexts rather than algorave,
;; i suppose ...
(defun inner-lifemodel (growth-cycle lifespan rest)
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
		   :name (intern (format nil "~D-lifemodel" (name wrapped-processor)))
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

(defun lifemodel (growth-cycle lifespan &rest rest)
  (inner-lifemodel growth-cycle lifespan rest))

(defun lm (growth-cycle lifespan &rest rest)
  (inner-lifemodel growth-cycle lifespan rest))

(defun life (growth-cycle lifespan var method &optional proc)
  (if proc
      (if (typep proc 'function)
          (lambda (pproc) (life growth-cycle lifespan var method (funcall proc pproc)))
          (make-instance 'lifemodel-control
		         :name (intern (format nil "~D-lifemodel" (name proc)))
		         :wrapped-processor proc
		         :growth-cycle growth-cycle		 
		         :variance var		 
		         :method method
		         :durs nil
		         :phoe 4
		         :node-lifespan lifespan
		         :hoe-max 4
		         :exclude nil
		         :autophagia t
		         :apoptosis nil))
      (lambda (pproc) (life growth-cycle lifespan var method pproc))))
