;; stateful parameter modifier ... (yes, really ...)
;; every one of those needs an "evaluate" function ...
(defclass param-mod-object ()
  ((step :accessor pmod-step :initform 0)
   (time :accessor pmod-time :initform 0)
   (current-value :accessor pmod-current-value :initarg :current-value)))

;; before each evaluation, set time ...
(defmethod evaluate :before ((p param-mod-object))
  (setf (pmod-time p) (incudine:now)))

(require :incudine)
;; after each evaluation, increment step counter
(defmethod evaluate :after ((p param-mod-object))
  (incf (pmod-step p)))

;; this one is stateless, not dependent on current value ...
(defclass param-oscillate-between (param-mod-object) nil)

(defmethod evaluate ((o param-oscillate-between))
  (let* ((osc-range (- upper lower))		   
	 (degree-increment (/ 360 cycle o))
	 (degree (mod (* degree-increment (mod step cycle)) 360))
	 (abs-sin (abs (sin (radians degree)))))    
    (+ lower (* abs-sin osc-range))))

(defclass generic-brownian-motion ()
  (upper-boundary :accessor ubound :initarg :upper-boundary)
  (lower-boundary :accessor lbound :initarg :lower-boundary)
  (step-size :accessor step-size :initarg :step-size)
  (is-bounded :accessor is-bounded :initarg :is-bounded)
  (is-wrapped :accessor is-wrapped :initarg :is-wrapped))

;; cap or wrap ...
(defmethod cap ((b generic-brownian-motion) value &key)
  (cond ((is-bounded b)
	 (cond ((< value (lbound b)) (lbound b))
	       ((> value (ubound b)) (ubound b))
	       (t value)))
	((is-wrapped b)
	 (cond ((< value (lbound b)) (ubound b))
	       ((> value (ubound b)) (lbound b))
	       (t value)))
	(t value)))

;; this one is stateful ...
(defclass param-brownian-motion (generic-brownian-motion param-mod-object))

(defmethod evaluate ((b param-brownian-motion))
  (let* ((new-value (cap b (+ (pmod-current-value b) 
			      (* (nth (random 2) '(-1 1)) (step-size b))))))
    ;; stateful - don't forget to set value ! 
    (setf (pmod-current-value b) new-value)
    ;; return new value
    new-value))

;; the atomic units of music - event and transition ...
(defclass event ()
  ((source :accessor event-source)
   (tags :accessor event-tags :initarg :tags)
   (backends :accessor event-backends :initarg :backends :initform `(,*default-dsp-backend*))
   (value-combine-function :accessor value-combine-function
			   :initarg :combi-fun :initform #'replace-value)))

;; the default value combination function
(defun replace-value (b a) a)

;; DIRECTLY EVENT-RELEATED OBJECT HANDLING METHODS ...
(defmethod event-has-slot ((e event) slot &key)
  (member slot (class-slots (class-of e)) :test 'slot-eq))

(defmethod event-has-slot-by-name ((e event) slot-name  &key)
  (member slot-name (mapcar #'slot-definition-name (class-slots (class-of e)))))

;; check if event b has all slots that event a has
(defmethod events-compatible ((a event) (b event) &key)
  (subsetp (class-slots (class-of a)) (class-slots (class-of b)) :test 'slot-eq))

(defmethod overwrite-slots ((a event) (b event) &key)
  (loop for slot in (class-slots (class-of a))
     do (when (slot-boundp-using-class (class-of b) b slot)
	  (unless (member (slot-definition-name slot) *protected-slots*)
	    (setf (slot-value b (slot-definition-name slot))
		  (funcall (value-combine-function a) (slot-value b (slot-definition-name slot))
			   (slot-value a (slot-definition-name slot))))))) b)

(defmethod copy-slots-to-class ((a event) (b event) &key)
  (loop for slot in (class-direct-slots (class-of a))
     do (unless (event-has-slot b slot)
	  (add-slot-to-class (class-name (class-of b)) (slot-definition-name slot)
			     :readers (slot-definition-readers slot)
			     :writers (slot-definition-writers slot)))))

;; will be an accumulator ... 
(defclass incomplete-event (event) ())

;; copied ...
(defun interleave (l1 l2)
  (cond ((and (eql l1 nil) (eql l2 nil)) nil)             ;; rule #1 
        ((eql l1 nil) (cons nil (interleave l2 l1)))      ;; rule #2, current value is nil
        (t (cons (first l1) (interleave l2 (rest l1)))))) ;; rule #3 in all other cases

;; helper to create 
(defun create-accessor (class-name accessor-name param-name)
  `(defgeneric ,accessor-name (,class-name)
    (:method ((,class-name ,class-name))
      (let ((val (slot-value ,class-name ',param-name)))
	(if (typep val 'param-mod-object)
	    (evaluate val)
	    val)))))

(defun get-param-definition (slot)
  (list
   (sb-mop::slot-definition-name slot)
   (car (sb-mop::slot-definition-readers slot))
   (sb-mop::slot-definition-initform slot)))

(defun get-param-definitions (event-class)
  (if (member (find-class 'event) (sb-mop::class-direct-superclasses event-class))
      (mapcar #'get-param-definition (sb-mop::class-direct-slots event-class))
      (mapcan #'get-param-definitions (sb-mop::class-direct-superclasses event-class))))

;; a overwrites b, b (or incomplete) is returned ...
(defmethod combine-single-events ((a event) (b event) &key)
  (cond ((events-compatible a b) (overwrite-slots a b))
	;; merge events into a new incomplete event
	(t (let ((new-event (make-instance 'incomplete-event)))
	      (copy-slots-to-class a new-event)
	      (copy-slots-to-class b new-event)
	      (overwrite-slots b new-event)
	      (overwrite-slots a new-event)
	      ))))

;; combining events ... a has precedence
(defmethod combine-events (events-a events-b &key (mode 'append) (filter #'all-p))
  (cond ((eq mode 'append) (append events-a events-b))
	((eq mode 'zip) (let ((filtered-and-combined
			       (mapcar #'combine-single-events events-a
				       (remove-if-not filter events-b)))
			      (rest (remove-if filter events-b)))
			  (append filtered-and-combined rest)))))


;; creepy macro to faciliate defining events
;; defines the event class, the language constructor, and the
;; value accessor function ...
(defmacro define-event (&key
			  short-name
			  long-name
			  (parent-events nil)
			  (parameters nil)
			  (direct-parameters nil)
			  (handler nil))
  (let* ((class-name (intern (format nil "~A" long-name)))
	 (keyword-parameters  (remove-if #'(lambda (x) (member (car x) direct-parameters)) parameters))
	 ;; get parameter definitions from parent classes ...
	 (parent-parameters (mapcan #'(lambda (cl)
					(get-param-definitions (find-class cl)))
				    parent-events))
	 (parent-keyword-parameters (remove-if #'(lambda (x) (member (car x) direct-parameters)) 
					       parent-parameters))
	 (parameter-names (mapcar #'car parameters))
	 (accessor-names (mapcar #'cadr parameters))
	 (keyword-parameter-defaults (mapcar #'caddr keyword-parameters))
	 (keyword-parameter-names (mapcar #'car  keyword-parameters))
	 
	 (parent-parameter-names (mapcar #'car  parent-parameters))

	 (parent-keyword-parameter-defaults (mapcar #'caddr parent-keyword-parameters))
	 (parent-keyword-parameter-names (mapcar #'car  parent-keyword-parameters))
	 

	 (keywords (mapcar #'(lambda (x) (intern (format nil "~A" x) "KEYWORD")) parameter-names))
	 (parent-keywords (mapcar #'(lambda (x) (intern (format nil "~A" x) "KEYWORD"))
				  parent-parameter-names))
	 (keyword-pairs (interleave keywords parameter-names))
	 (parent-keyword-pairs (interleave parent-keywords parent-parameter-names))
	 (class-name-list (make-list (length parameter-names) :initial-element class-name)))
    `(progn
       ;; define the base class
       (defclass ,class-name ,parent-events ())
       ;; add the parameter slots with accessor  
       (loop for param in ',parameters	    
	  for i from 0 to (length ',parameters)
	  do (let* ((slot-name (car param))
		    (slot-keyword (intern (format nil "~A" slot-name) "KEYWORD"))
		    (slot-initform (caddr param))
		    (accessor-name (cadr param)))
	       (add-slot-to-class ',class-name
				  slot-name
				  :accessors (list accessor-name)
				  :initargs (list slot-keyword)
				  :initform slot-initform)))
       ;; define the constructor function
       (defun ,short-name (,@direct-parameters
				 &key
				   ,@(mapcar #'list keyword-parameter-names
					     keyword-parameter-defaults)
				   ,@(mapcar #'list parent-keyword-parameter-names
					     parent-keyword-parameter-defaults)
				   (tags nil)
				   (combi-fun #'replace-value))
	 (make-instance ',class-name
			,(intern "TAGS" "KEYWORD") tags
			,(intern "COMBI-FUN" "KEYWORD") combi-fun
			,@keyword-pairs
			,@parent-keyword-pairs
			))
       ;; oh my ... now this is creepy ...
       ;; re-define the getters so that the value is calculated if
       ;; it's a modifier object instead of a plain value ...
       ,@(mapcar #'create-accessor class-name-list accessor-names parameter-names)
       ;; tbd -- directly include handler ... 
       )))



				        


