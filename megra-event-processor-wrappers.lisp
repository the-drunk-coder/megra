(in-package :megra)

(defclass event-processor-wrapper (event-processor)
  ((wrapped-processor :accessor wrapper-wrapped-processor :initarg :wrapped-processor)))

;; pass everything on to inner processor 
(defmethod push-tmod ((w event-processor-wrapper) tmod &key)
  (push-tmod (wrapper-wrapped-processor w) tmod))

(defmethod pop-tmod ((w event-processor-wrapper) &key)
  (pop-tmod (wrapper-wrapped-processor w)))

(defmethod set-current-node ((w event-processor-wrapper) cnode &key)
  (set-current-node (wrapper-wrapped-processor w) cnode))

(defmethod set-traced-path ((w event-processor-wrapper) tpath &key)
  (set-traced-path (wrapper-wrapped-processor w) tpath))

(defmethod current-node ((w event-processor-wrapper))
  (current-node (wrapper-wrapped-processor w)))

(defmethod traced-path ((w event-processor-wrapper))
  (traced-path (wrapper-wrapped-processor w)))

(defmethod last-transition ((w event-processor-wrapper))
  (last-transition (wrapper-wrapped-processor w)))

(defmethod inner-generator ((w event-processor-wrapper))
  (inner-generator (wrapper-wrapped-processor w)))

(defmethod ages ((w event-processor-wrapper))
  (ages (wrapper-wrapped-processor w)))

(defmethod name ((w event-processor-wrapper))
  (name (wrapper-wrapped-processor w)))

(defmethod trace-length ((w event-processor-wrapper))
  (trace-length (wrapper-wrapped-processor w)))

(defmethod pull-events ((w event-processor-wrapper) &key)
  (if (successor w)
      (apply-self w (pull-events (successor w)))
      (current-events w)))

(defmethod pull-events :after ((w event-processor-wrapper) &key)
  (post-processing w))

;; pass through apply-self ?
(defmethod current-events ((w event-processor-wrapper) &key)
  (pull-events (wrapper-wrapped-processor w)))

(defmethod current-transition ((w event-processor-wrapper) &key)
  (pull-transition (wrapper-wrapped-processor w)))

(defmethod combine-filter ((w event-processor-wrapper))
  (combine-filter (wrapper-wrapped-processor w)))

(defmethod grow ((w event-processor-wrapper)
                 &key (var 0)		        
		      durs
		      functors
		      (method 'old)
		      (rnd 0)
		      higher-order)
  (grow (wrapper-wrapped-processor w) :var var :durs durs :functors functors
                                      :method method :rnd rnd :higher-order higher-order))

(defmethod prune ((w event-processor-wrapper) &key exclude node-id)
  (prune (wrapper-wrapped-processor w) :exclude exclude :node-id node-id))

(defmethod post-processing ((w event-processor-wrapper) &key))

;;;;;;;;;;;;;;;; GENERIC Population Control ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; population here refers to the number of symbols in a generator's alphabet ...
;; implementations of this can be found in 'megra-lifemodel' and 'megra-probctrl'

(defclass generic-population-control ()
  ((variance :accessor population-control-var :initarg :variance)
   (method :accessor population-control-method :initarg :method)
   (durs :accessor population-control-durs :initarg :durs)
   (phoe :accessor population-control-higher-order-probability :initarg :phoe)
   (hoe-max :accessor population-control-higher-order-max-order :initarg :hoe-max)
   (exclude :accessor population-control-exclude :initarg :exclude)))

;; helper function for shorthands ...
(defun find-keyword-val (keyword seq &key default)
  (if (and
       (member keyword seq)
       (> (length (member keyword seq)) 0) ;; check if there's chance the keyword has a value ...
       (not (eql (type-of (cadr (member keyword seq))) 'keyword)))
      (let* ((pos (position keyword seq))
	     (val (nth (+ pos 1) seq)))
	val)
      default))

;;;;;;;;;;;;;;;;;;;;;;;;;;; SOME RANDOM PROCESSOR WRAPPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;         NEEDS CLEAN-UP          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun evr (count fun &optional proc)
  (if proc            
      (make-instance 'count-wrapper
                     :on-count count 
                     :function fun
                     :wrapped-processor (if (functionp proc) (funcall proc) proc))
      (lambda (pproc) (evr count fun pproc))))

;; prob
(defclass prob-wrapper (event-processor-wrapper)
  ((prob :accessor prob-wrapper-prob :initarg :prob)
   (function :accessor prob-control-function :initarg :function)))

(defmethod post-processing ((p prob-wrapper) &key)
  (when (< (random 100) (prob-wrapper-prob p))
    (funcall (prob-control-function p) (wrapper-wrapped-processor p))))

(defun pprob (prob fun &optional aproc)
  (let ((proc (if (or (typep aproc 'function)
                      (typep aproc 'event-processor))
                  aproc)))
    (if proc
        (lambda () (make-instance 'prob-wrapper                         
                             :prob prob 
                             :function fun                
                             :wrapped-processor (if (typep proc 'event-processor)
                                                    proc
                                                    (funcall proc))))
        (lambda (wproc) (pprob prob fun wproc)))))

(defclass applicator (event-processor-wrapper)
  ((events-to-apply :accessor applicator-events :initarg :events)))

(defmethod pull-events ((w applicator) &key)
  (if (successor w)
      (let ((other-events (current-events w)))
        (loop for aev in (applicator-events w)
              do (loop for i from 0 to (- (length other-events) 1)
                       do (setf (nth i other-events)
                                (combine-single-events aev (nth i other-events)))))
        (apply-self-2 w other-events (pull-events (successor w))))      
      (let ((other-events (current-events w)))
        (loop for aev in (applicator-events w)
              do (loop for i from 0 to (- (length other-events) 1)
                       do (setf (nth i other-events)
                                (combine-single-events aev (nth i other-events)))))
        other-events)))

(defun pear (&rest events-and-proc)
  (let ((proc (if (or (typep (car (last events-and-proc)) 'function)
                      (typep (car (last events-and-proc)) 'event-processor))
                  (car (last events-and-proc)))))
    (if proc
        (lambda () (make-instance 'applicator                        
                             :events (butlast events-and-proc)
                             :wrapped-processor (if (typep proc 'event-processor)
                                                    proc
                                                    (funcall proc))))
        (lambda (wproc) (apply 'pear (nconc events-and-proc (list wproc)))))))

(defclass prob-applicator (event-processor-wrapper)
  ((prob-event-mapping :accessor prob-mapping :initarg :mapping)))

(defmethod pull-events ((w prob-applicator) &key)
  (if (successor w)    
      (let ((other-events (current-events w)))
        (loop for prob being the hash-keys of (prob-mapping w) using (hash-value events)
              when (< (random 100) prob)
              do (loop for aev in events
                       do (loop for i from 0 to (- (length other-events) 1)
                                do (setf (nth i other-events)
                                         (combine-single-events aev (nth i other-events))))))      
        (apply-self-2 w other-events (pull-events (successor w))))
      (let ((other-events (current-events w)))
        (loop for prob being the hash-keys of (prob-mapping w) using (hash-value events)
              when (< (random 100) prob)
              do (loop for aev in events
                       do (loop for i from 0 to (- (length other-events) 1)
                                do (setf (nth i other-events)
                                         (combine-single-events aev (nth i other-events))))))
        other-events)))

(defun ppear (&rest params)
  (let* ((proc (if (or (typep (alexandria::lastcar params) 'event-processor)
                       (typep (alexandria::lastcar params) 'function))
                   (alexandria::lastcar params)))         
         (mapping (probability-list-hash-table (if proc (butlast params) params))))
    (if proc
        (lambda () (make-instance 'prob-applicator :mapping mapping :wrapped-processor (if (functionp proc) (funcall proc) proc)))
        (lambda (pproc) (apply 'ppear (nconc params (list pproc)))))))

