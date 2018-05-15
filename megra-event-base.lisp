;; the atomic units of music - events and transitions ...
(defclass event ()
  ((source :accessor event-source)
   (tags :accessor event-tags :initarg :tags)
   (backends :accessor event-backends
	     :initarg :backends
	     :initform `(,*default-dsp-backend*))
   (value-combine-function :accessor value-combine-function
			   :initarg :combi-fun
			   :initform #'replace-value)))

;; upper and lower limits for parameters. Refer to those when using
;; "blind" modificators that don't set explicit limits ... 
(defparameter *parameter-limits* (make-hash-table :test 'equal))

;; the default value combination function
(defun replace-value (b a) a)

;; see what we can still do with this ... 
(defmethod handle-event ((e event) timestamp &key))

;; DIRECTLY EVENT-RELEATED OBJECT HANDLING METHODS ...
(defmethod event-has-slot ((e event) slot &key)
  (member slot (class-slots (class-of e)) :test 'slot-eq))

(defmethod event-has-slot-by-name ((e event) slot-name  &key)
  (member slot-name (mapcar #'slot-definition-name (class-slots (class-of e)))))

;; check if event b has all slots that event a has
(defmethod events-compatible ((a event) (b event) &key)
  (subsetp (class-slots (class-of a)) (class-slots (class-of b)) :test 'slot-eq))

(defun eval-slot-value (slot-value)
  (cond ((typep slot-value 'param-mod-object) (evaluate slot-value))
	((typep slot-value 'function) (funcall slot-value))
	(t slot-value)))

(defmethod overwrite-slots ((a event) (b event) &key)
  (loop for slot in (class-slots (class-of a))
     do (when (slot-boundp-using-class (class-of b) b slot)
	  (unless (member (slot-definition-name slot) *protected-slots*)
	    (setf (slot-value b (slot-definition-name slot))
		  (funcall (value-combine-function a)
			   (eval-slot-value
			    (slot-value b (slot-definition-name slot)))
			   (eval-slot-value
			    (slot-value a (slot-definition-name slot)))))))) b)

(defmethod copy-slots-to-class ((a event) (b event) &key)
  (loop for slot in (class-direct-slots (class-of a))
     do (unless (event-has-slot b slot)
	  (add-slot-to-class (class-name (class-of b)) (slot-definition-name slot)
			     :readers (slot-definition-readers slot)
			     :writers (slot-definition-writers slot)))))

;; copied ... interleave two lists ...(a1 b1) (a2 b2) -> (a1 a2 b1 b2)
(defun interleave (l1 l2)
  (cond ((and (eql l1 nil) (eql l2 nil)) nil)         ;; rule #1 
        ((eql l1 nil) (cons nil (interleave l2 l1)))  ;; rule #2, current val is nil
        (t (cons (first l1) (interleave l2 (rest l1)))))) ;; rule #3 all other cases

;; helper to create 
(defun create-accessor (class-name accessor-name param-name)
  `(defgeneric ,accessor-name (,class-name)
    (:method ((,class-name ,class-name))
      (eval-slot-value (slot-value ,class-name ',param-name)))))

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
    (cond ((typep b 'control-event) b) ;; let control events untouched ... 
	  ((events-compatible a b) (overwrite-slots a b))	  
	  ;; merge events into a new incomplete event
	  (t (let ((new-event (make-instance 'incomplete-event)))
	      (copy-slots-to-class a new-event)
	      (copy-slots-to-class b new-event)
	      (overwrite-slots b new-event)
	      (overwrite-slots a new-event)))))

;; combining events ... a has precedence
(defmethod combine-events (events-a events-b &key (mode 'append) (filter #'all-p))
  (cond ((eq mode 'append) (append events-a events-b))
	((eq mode 'zip) (mapc
			 #'(lambda (ev-b ev-a)
			     (if (funcall filter ev-b)
				 (combine-single-events ev-a ev-b)
				 ev-b))
			 ;; got to be in this order as mapc returns first list 
			 events-b
			 events-a))))

;; helper methods to turn events back into their textual representation ...
(defun print-tags (tags)
  (if tags
      (let ((tags-string (string-trim '(#\Space) (format nil "~{~a ~}" tags))))
	(format nil ":tags '(~a) " tags-string)) ""))

;; helper method to print combi function name 
(defun print-combi-fun (fun)
  ;; sbcl-specific ??
  (format nil ":combi-fun #'~a" (print-function-name fun)))

;; generic helper method to print function name ...
(defun print-function-name (fun)
  (format nil "~a"
	  (nth 2 (multiple-value-list
		  (function-lambda-expression fun)))))

;; some parameters are assembled from other parameters and should not be printed
(defparameter *dont-print-this-keyword-parameters* '(sample-location))

;; format specific parameters 
(defun print-param (param-name param-value is-keyword-param)
  (let ((value-string (cond ((eql param-name 'pitch) 
			     (if (typep param-value 'integer)
				 (format nil "~D" param-value)
				 (format nil "'~a" param-value)))
			    ((typep param-value 'string)
			     (format nil "\"~a\"" param-value))
			    (t (format nil "~a" param-value)))))
    (if is-keyword-param
	(if (member param-name *dont-print-this-keyword-parameters*)
	    ""
	    (format nil ":~a ~a " param-name param-value))	
	(format nil "~a " value-string))))

;; creepy macro to faciliate defining events
;; defines the event class, the language constructor, and the
;; value accessor function ...
(in-package :megra)
(defmacro define-event (&key
			  short-name
			  long-name
			  (parent-events nil)
			  (parameters nil)
			  (direct-parameters nil)
			  (create-accessors t)
			  (handler nil))
  (let* ((class-name (intern (format nil "~A" long-name)))
	 (keyword-parameters  (remove-if #'
			       (lambda (x)
				 (member (car x) direct-parameters)) parameters))
	 ;; get parameter definitions from parent classes ...
	 (parent-parameters (mapcan #'(lambda (cl)
					(get-param-definitions (find-class cl)))
				    parent-events))
	 (direct-parameter-defs (nconc (remove-if-not #'
					(lambda (x)
					  (member (car x) direct-parameters))
					parameters)
				       (remove-if-not #'
					(lambda (x)
					  (member (car x) direct-parameters))
					parent-parameters))) 
	 (parent-keyword-parameters (remove-if #'(lambda (x)
						   (member (car x)
							   direct-parameters)) 
					       parent-parameters))
	 (parameter-names (mapcar #'car parameters))
	 (accessor-names (mapcar #'cadr parameters)) ;; second is a accessor name 
	 (keyword-parameter-defaults
	  (mapcar (lambda (l) (nth 2 l)) keyword-parameters)) ;; 3rd is default value    
	 (keyword-parameter-names
	  (mapcar #'car keyword-parameters)) ;; first is name 
	 (parent-parameter-names (mapcar #'car parent-parameters))
	 (parent-keyword-parameter-defaults
	  (mapcar (lambda (l) (nth 2 l))
		  parent-keyword-parameters)) ;; 3rd is default	 
	 (parent-keyword-parameter-names (mapcar #'car parent-keyword-parameters))
	 (keywords
	  (mapcar #'(lambda (x)
		      (intern (format nil "~A" x) "KEYWORD")) parameter-names))
	 (parent-keywords
	  (mapcar #'(lambda (x) (intern (format nil "~A" x) "KEYWORD"))
		  parent-parameter-names))
	 (keyword-pairs (interleave keywords parameter-names))
	 (parent-keyword-pairs (interleave parent-keywords parent-parameter-names))
	 (class-name-list (make-list (length parameter-names)
				     :initial-element class-name)))
    `(progn
       ;; define the base class
       (defclass ,class-name ,parent-events ())
       ;; add the parameter slots with accessor ...
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
				  :initform slot-initform)
	       ;; set parameter limits
	       (setf (gethash slot-name *parameter-limits*)
		     (list (nth 3 param) (nth 4 param)))))
       ;; define the constructor function
       (defun ,short-name (,@direct-parameters
				 &key
				   ,@(mapcar #'list keyword-parameter-names
					     keyword-parameter-defaults)
				   ,@(mapcar #'list parent-keyword-parameter-names
					     parent-keyword-parameter-defaults)
				   (backends '(,*default-dsp-backend*))
				   (tags nil)
				   (combi-fun #'replace-value))
	 (make-instance ',class-name
			;; add the very basic keyword parameters 'by hand'
			,(intern "BACKENDS" "KEYWORD") backends
			,(intern "TAGS" "KEYWORD") tags
			,(intern "COMBI-FUN" "KEYWORD") combi-fun
			,@keyword-pairs
			,@parent-keyword-pairs
			))
       ;; oh my ... now this is creepy ...
       ;; re-define the getters so that the value is calculated if
       ;; it's a modifier object or function instead of a plain value ...
       (if ,create-accessors
	   (progn ,@(mapcar #'create-accessor
			    class-name-list accessor-names parameter-names)))
       ;; produce event handler method ...
       (defmethod handle-event ((evt ,class-name) timestamp &key)
	 (handler-case ,handler
	   (simple-error (e) (incudine::msg error "~D" e))))
       ;; assemble printer method ...              
       (defmethod print-event ((evt ,class-name) &key)
	 (string-downcase (format nil "(~a ~{~a~}~{~a~}~{~a~}~a~a)"
		 ',short-name
		 (mapcar #'(lambda (par-name direct-accs-name)
			     (print-param par-name
					  (funcall direct-accs-name evt) nil))
			 ',(mapcar #'car direct-parameter-defs)
			 ',(mapcar #'cadr direct-parameter-defs))
		 (mapcar #'(lambda (par-name accs-name)
			     (print-param par-name (funcall accs-name evt) t))
			     ',keyword-parameter-names
			     ',(mapcar #'cadr keyword-parameters))
		 (mapcar #'(lambda (par-name accs-name)
			     (print-param par-name (funcall accs-name evt) t))
			 ',parent-keyword-parameter-names
			 ',(mapcar #'cadr parent-keyword-parameters))
		 (print-tags (event-tags evt))
		 (print-combi-fun (value-combine-function evt))))))))
;; end event definition macro ...

;; another macro if you want to make an event available under a different
;; constructor, with different direct parameters and different defaults
;; the print-function will be the original one ...
;;(in-package :megra)
(defmacro define-event-alias (&key
				alias
				long-name				
				(direct-parameters nil)
				(alias-defaults nil))
  (let* ((event-class (find-class long-name))
	 ;;(parent-events (mapcar #'class-name (sb-mop::class-direct-superclasses event-class)))
	 (parameters (remove-duplicates (nconc (get-param-definitions event-class)
					       (mapcar #'get-param-definition
						       (sb-mop::class-direct-slots event-class)))
					:test #'equal))
	 (keyword-parameters  (remove-if #'(lambda (x) (member (car x) direct-parameters)) parameters))
	 ;;(direct-parameter-defs (remove-if-not #' (lambda (x) (member (car x) direct-parameters))
	;;					  parameters))    
	 (parameter-names (mapcar #'car parameters))
	 ;;(accessor-names (mapcar #'cadr parameters))
	 (alias-default-names (mapcar #'car alias-defaults))
	 (keyword-parameter-defaults (mapcar #'(lambda (param-def)
						 (if (member (car param-def) alias-default-names)
						     (cadr (assoc (car param-def) alias-defaults))
						     (caddr param-def)
						     )) keyword-parameters))
	 (keyword-parameter-names (mapcar #'car keyword-parameters))     	 
	 (keywords (mapcar #'(lambda (x) (intern (format nil "~A" x) "KEYWORD")) parameter-names))	 
	 (keyword-pairs (interleave keywords parameter-names))	 
	)
    `(progn
       ;; define the constructor function
       (defun ,alias (,@direct-parameters
			   &key
			     ,@(mapcar #'list keyword-parameter-names
				       keyword-parameter-defaults)			     
			     (tags nil)
			     (combi-fun #'replace-value))
	 (make-instance ',long-name
			,(intern "TAGS" "KEYWORD") tags
			,(intern "COMBI-FUN" "KEYWORD") combi-fun
			,@keyword-pairs)))))

;; helper functions for abstract sampling events ...
(defun keyword-match-score (keywords string)
  (if keywords
      (let ((matches (mapcar #'(lambda (keyword)
				 (search (symbol-name keyword)
					 (string-upcase string)))
			     keywords)))    
	(float (/ (length (remove nil matches)) (length keywords))))
      1.0))

(defun get-matching-sample-name (categ keywords)
  (let ((best-match "")
	(best-score 0.0))
    (loop for path in
       ;; shuffle list, so that we won't get the same thing
       ;; everytime no keywords are provided
	 (shuffle-list (cl-fad::list-directory (concatenate 'string cm::*sample-root*
							    (string-downcase categ))))
       do (let ((cur-score (keyword-match-score keywords (pathname-name path))))
	    (when (eql cur-score 1.0)
	      (setf best-match (pathname-name path))
	      (return))
	    (when (> cur-score best-score)
	      (setf best-score cur-score)
	      (setf best-match (pathname-name path)))))	      
    best-match))

(defmacro define-category-sampling-event (name dur)
  `(defun ,(read-from-string name) (&rest keywords)
     (grain (string-downcase ,name)
	    (get-matching-sample-name ,name keywords) :dur ,dur
	    :lvl 0.4 :rate 1.0 :start 0.00 :atk 1 :rel 7
	    :lp-dist 1.0 :lp-freq 5000 :rev 0.0 :pos 0.5 :tags '(,name))))


