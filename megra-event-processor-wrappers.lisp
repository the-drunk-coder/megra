(in-package :megra)

(defclass event-processor-wrapper (event-processor)
  ((filters-incl :accessor filters-incl :initarg :filters-incl)
   (filters-excl :accessor filters-excl :initarg :filters-excl)
   (wrapped-processor :accessor wrapper-wrapped-processor :initarg :wrapped-processor)))

(defmethod activate ((w event-processor-wrapper))
  (activate (wrapper-wrapped-processor w)))

(defmethod deactivate ((w event-processor-wrapper))
  (deactivate (wrapper-wrapped-processor w)))

;; pass everything on to inner processor 
(defmethod push-tmod ((w event-processor-wrapper) tmod &key)
  (push-tmod (wrapper-wrapped-processor w) tmod))

(defmethod pop-tmod ((w event-processor-wrapper) &key)
  (pop-tmod (wrapper-wrapped-processor w)))

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
  (current-events w))

(defmethod pull-events :after ((w event-processor-wrapper) &key)
  (post-processing w))

;; pass through apply-self ?
(defmethod current-events ((w event-processor-wrapper) &key)
  (pull-events (wrapper-wrapped-processor w)))

(defmethod current-transition ((w event-processor-wrapper) &key)
  (pull-transition (wrapper-wrapped-processor w)))

(defmethod combine-filter ((w event-processor-wrapper))
  (combine-filter (wrapper-wrapped-processor w)))

(defmethod grow-generator ((w event-processor-wrapper)
                           &key (var 0)		        
		                durs
		                functors
		                (method 'old)
		                (rnd 0)
		                higher-order)
  (grow-generator (wrapper-wrapped-processor w) :var var :durs durs :functors functors
                                      :method method :rnd rnd :higher-order higher-order))

(defmethod prune-generator ((w event-processor-wrapper) &key exclude node-id)
  (prune-generator (wrapper-wrapped-processor w) :exclude exclude :node-id node-id))

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
  (lambda (&optional next)      
    (cond ((not next)
           (make-instance 'count-wrapper
                          :on-count count 
                          :function fun
                          :wrapped-processor (if proc (funcall proc))))
          (proc (evr count fun (funcall proc next)))
          (t (evr count fun next)))))

;; prob
(defclass prob-wrapper (event-processor-wrapper)
  ((prob :accessor prob-wrapper-prob :initarg :prob)
   (function :accessor prob-control-function :initarg :function)))

(defmethod post-processing ((p prob-wrapper) &key)
  (when (< (random 100) (prob-wrapper-prob p))
    (funcall (prob-control-function p) (wrapper-wrapped-processor p))))

(defun apple (prob fun &optional aproc)
  (let ((proc (if (typep aproc 'function) aproc)))
    (lambda (&optional next)      
      (cond ((not next)
             (make-instance 'prob-wrapper
                            :prob prob 
                            :function fun                
                            :wrapped-processor (if proc (funcall proc))))
            (proc (apple prob fun (funcall proc next)))
            (t (apple prob fun next))))))

(defclass applicator (event-processor-wrapper)
  ((events-to-apply :accessor applicator-events :initarg :events)))

(defmethod pull-events ((w applicator) &key)
  (let* ((other-events (current-events w))
         (filtered-events (cond ((filters-incl w) (remove-if-not (filters-incl w) other-events))
                                ((filters-excl w) (remove-if (filters-excl w) other-events))
                                (t other-events)))
         (rem-events (remove-if #'(lambda (ev) (member ev filtered-events)) other-events)))    
    (loop for aev in (applicator-events w)
          do (loop for i from 0 to (- (length filtered-events) 1)
                   do (setf (nth i filtered-events)
                            (combine-single-events aev (nth i filtered-events)))))
    (nconc rem-events filtered-events)))

(defmethod pull-transition ((w applicator) &key)
  (if (affect-transition w)
      (let ((cur-trans (car (current-transition w))))
        (loop for aev in (applicator-events w) do (combine-single-events aev cur-trans))
        (list cur-trans))      
      (current-transition w)))

(defun pear (&rest events-and-proc)
  (let ((filters-incl (find-keyword-symbol-list :for events-and-proc))
        (filters-excl (find-keyword-symbol-list :notfor events-and-proc))        
        (proc (if (functionp (car (last events-and-proc)))                      
                  (car (last events-and-proc)))))
    (lambda (&optional next)      
      (cond ((not next)             
             (make-instance 'applicator                        
                            :filters-incl (if filters-incl (multi-filter filters-incl))
                            :filters-excl (if filters-excl (multi-filter filters-excl))
                            :affect-transition (and (not (member 'transition filters-excl))
                                                    (member 'transition filters-incl))
                            :events (delete-if #'(lambda (i) (or (member i filters-incl)
                                                            (member i filters-excl)
                                                            (member i '(:for :notfor))))
                                               (if proc (butlast events-and-proc) events-and-proc))
                            :wrapped-processor (if proc (funcall proc))))
            (proc (apply 'pear (nconc (butlast events-and-proc) (list (funcall proc next)))))
            (t (apply 'pear (nconc events-and-proc (list next))))))))

;; more sane alias ..
(setf (fdefinition 'always) #'pear)

(defclass prob-applicator (event-processor-wrapper)
  ((prob-event-mapping :accessor prob-mapping :initarg :mapping)))

(defmethod pull-events ((w prob-applicator) &key)
  (let* ((other-events (current-events w))
         (filtered-events (cond ((filters-incl w) (remove-if-not (filters-incl w) other-events))
                                ((filters-excl w) (remove-if (filters-excl w) other-events))
                                (t other-events)))
         (rem-events (remove-if #'(lambda (ev) (member ev filtered-events)) other-events)))    
    (loop for prob being the hash-keys of (prob-mapping w) using (hash-value events)
          when (< (random 100) (if (numberp prob) prob (evaluate prob)))
          do (loop for aev in events
                   do (loop for i from 0 to (- (length filtered-events) 1)
                            do (setf (nth i filtered-events)
                                     (combine-single-events aev (nth i filtered-events))))))
    (nconc rem-events filtered-events)))

(defmethod pull-transition ((w prob-applicator) &key)
  (if (affect-transition w)
      (let ((cur-trans (car (current-transition w))))
        (loop for prob being the hash-keys of (prob-mapping w) using (hash-value events)
              when (< (random 100) (if (numberp prob) prob (evaluate prob)))
              do (loop for aev in events do (combine-single-events aev cur-trans)))        
        (list cur-trans))      
      (current-transition w)))

(defun ppear (&rest params)
  (let* ((filters-incl (find-keyword-symbol-list :for params))
         (filters-excl (find-keyword-symbol-list :notfor params))     
         (proc (if (typep (alexandria::lastcar params) 'function)
                   (alexandria::lastcar params)))         
         (mapping (probability-list-hash-table (delete-if #'(lambda (i) (or (member i filters-incl)
                                                                       (member i filters-excl)
                                                                       (member i '(:for :notfor))))
                                                          (if proc (butlast params) params)))))
    (lambda (&optional next)      
      (cond ((not next)
             (make-instance 'prob-applicator
                            :filters-incl (if filters-incl (multi-filter filters-incl))
                            :filters-excl (if filters-excl (multi-filter filters-excl))
                            :affect-transition (and (not (member 'transition filters-excl))
                                                    (member 'transition filters-incl))
                            :mapping mapping
                            :wrapped-processor (if proc (funcall proc))))
            (proc (apply 'ppear (nconc (butlast params) (list (funcall proc next)))))
            (t (apply 'ppear (nconc params (list next))))))))

;; more sane but less cute ...
(setf (fdefinition 'prob) #'ppear)

;; some shorthands
(defun inh (p &rest filters)
  (apply 'prob (nconc (list :for) filters (list p (lvl 0.0)))))

(defun exh (p &rest filters)
  (apply 'prob (nconc (list :notfor) filters (list p (lvl 0.0)))))

(defun inexh (p &rest filters)
  (cmp
   (apply 'prob (nconc (list :for) filters (list p (lvl 0.0))))
   (apply 'prob (nconc (list :notfor) filters (list p (lvl 0.0))))))


