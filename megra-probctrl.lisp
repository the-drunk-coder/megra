(in-package :megra)

;;;;;;;;;;;;;;;; Simple Probablistic Population Control ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; grow and prune the graph with a certain probability

(defclass probability-population-control (event-processor-wrapper
					  generic-population-control)
  ((pgrowth :accessor population-control-pgrowth :initarg :pgrowth)
   (pprune :accessor population-control-pprune :initarg :pprune)))

(defmethod post-processing ((g probability-population-control) &key)
  (when (< (random 100) (population-control-pgrowth g))
    (grow (wrapper-wrapped-processor g)
	  :var (population-control-var g)
	  :durs (population-control-durs g)
	  :method (population-control-method g)
	  :higher-order (if (< (random 100)
			       (population-control-higher-order-probability g))
		            (+ 2 (random
			          (- (population-control-higher-order-max-order g) 2)))
		            0)))
  (when (< (random 100) (population-control-pprune g))
    (prune (wrapper-wrapped-processor g) :exclude (population-control-exclude g))))

(defun pctrl (pgrowth pprune &rest rest)
  (let ((method (find-keyword-val :method rest :default 'triloop))
	(variance (find-keyword-val :var rest :default 0.2))
	(durs (find-keyword-val :durs rest :default nil))
	(hoe-max (find-keyword-val :hoe-max rest :default 4))
	(hoe (find-keyword-val :hoe rest :default 4))
	(exclude (find-keyword-val :exclude rest :default nil))
        (proc (if (or (typep (alexandria::lastcar rest) 'event-processor)
                      (typep (alexandria::lastcar rest) 'function))
                  (alexandria::lastcar rest))))
    (if proc
        (lambda () (make-instance 'probability-population-control
		             :wrapped-processor (if (functionp proc) (funcall proc) proc)
		             :variance variance
		             :pgrowth pgrowth
		             :pprune pprune
		             :method method
		             :durs durs
		             :phoe hoe
		             :hoe-max hoe-max
		             :exclude exclude))
        (lambda (pproc) (apply 'pctrl pgrowth pprune (nconc rest (list pproc)))))))
