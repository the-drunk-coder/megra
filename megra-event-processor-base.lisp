(in-package :megra)

;; generic event-processor
(defclass event-processor ()
  ((pull-events)
   (pull-transition)       
   (successor :accessor successor :initform nil)
   (predecessor :accessor predecessor :initform nil)   
   (current-events)      ;; abstract
   (current-transition)  ;; abstract   
   (chain-bound :accessor chain-bound :initform nil)   
   (name :accessor name :initarg :name)
   (clones :accessor clones :initform nil)
   (combine-mode :accessor combine-mode :initarg :combine-mode :initform 'auto)
   (affect-transition :accessor affect-transition :initarg :affect-transition :initform nil)
   (update-clones :accessor update-clones :initarg :update-clones :initform nil)))

(defmethod pull-events ((e event-processor) &key)
  (if (successor e)
      (apply-self e (pull-events (successor e)))
      (current-events e)))

;; events are the successor events 
(defmethod apply-self ((g event-processor) events &key)
  (combine-events (current-events g) events :mode (combine-mode g) :filter (combine-filter g)))

(defmethod apply-self-transition ((g event-processor) current-transition transition &key)
  (combine-events current-transition transition :mode (combine-mode g) :filter (combine-filter g)))

(defmethod pull-transition ((e event-processor) &key)
  (if (successor e)
      (let ((cur-trans (current-transition e)))
	(if (affect-transition e)
	    (apply-self-transition e cur-trans (pull-transition (successor g)))
	    (pull-transition (successor e))))
      (current-transition e)))

;; pass -- default 
(defmethod current-transition ((m event-processor) &key))

;; dummy processor for testing, development and debugging ..
(defclass dummy-event-processor (event-processor)
  ((name :accessor dummy-name)))

(defmethod apply-self ((e dummy-event-processor) events &key)
  (fresh-line)
  (princ "applying ")
  (current-events e))

(defmethod current-events ((e dummy-event-processor) &key)
  (fresh-line)
  (princ "dummy events from ")
  (princ (dummy-name e)))

(defmethod current-transition ((e dummy-event-processor) &key)
  (fresh-line)
  (princ "dummy transition from ")
  (princ (dummy-name e)))

(defclass processor-chain (event-processor)
  ((topmost-processor :accessor topmost-processor :initarg :topmost)
   (synced-chains :accessor synced-chains :initform nil)
   (synced-progns :accessor synced-progns :initform nil)
   ;; think of anschluss-zug -> connection train ... 
   (anschluss-kette :accessor anschluss-kette :initform nil) 
   (wait-for-sync :accessor wait-for-sync :initform nil)
   (active :accessor is-active :initform nil :initarg :is-active)
   (shift :accessor chain-shift :initform 0.0 :initarg :shift)
   (group :accessor chain-group :initform nil :initarg :group)))

(defun activate (chain)
  (incudine::msg info "activating ~D" chain)
  (setf (wait-for-sync chain) nil)
  (setf (is-active chain) t))

;; deactivate ... if it's a modifying event processor, delete it ...
(defun deactivate (chain)
  (incudine::msg info "deactivating ~D" chain)
  (setf (wait-for-sync chain) nil)
  (setf (is-active chain) nil))

(defmethod pull-events ((p processor-chain) &key)
  (pull-events (topmost-processor p)))

(defmethod pull-transition ((p processor-chain) &key)
  (pull-transition (topmost-processor p)))

(defun detach (processor)
  (when processor
    (when (predecessor processor)
      (detach (predecessor processor))
      (setf (predecessor processor) nil))
    (when (successor processor)
      (setf (successor processor) nil))    
    (setf (chain-bound processor) nil)))

(defun connect (processor-ids chain-name)
  (let ((current (car processor-ids))
	(next (cadr processor-ids)))
    ;; if you try to hook it into a different chain ... 
    (when (and next 
	       (chain-bound next)
	       (not (eql (chain-bound next) chain-name)))      
      (incudine::msg
       error
       "detaching ~D, already bound ..."
       (cadr processor-ids))
      ;; revert the work that has been done so far ... 
      (detach next))
    (when next
      ;; if processor already has predecessor, it means that it is already
      ;; bound in a chain ... 		
      (setf (successor current) next)
      (setf (predecessor next) current)	  
      (connect (cdr processor-ids) chain-name))
    (setf (chain-bound current) chain-name)))

(defun gen-proc-name (ch-name proc idx)
  (intern (concatenate 'string
		       (string ch-name) "-"
		       (string (class-name (class-of proc))) "-"
		       (format nil "~d" idx))))

;; handle the processor list ...
(defun gen-proc-list (ch-name proc-list)
  (let ((idx 0))
    (mapcar #'(lambda (proc)
		(incf idx)	        
		(cond ((typep proc 'symbol)
		       (gethash proc *processor-directory*))
		      ;; check if proc is already present,
		      ;; if not, name it and insert it
		      ;; the proc constructor will check if
		      ;; there's
		      ;; an old instance of itself,
		      ;; and replace itself in that case
		      ((and (not (typep proc 'graph-event-processor))
			    (not (typep proc 'mpfa-event-processor))
			    (not (gethash (name proc) *processor-directory*)))
		       (let ((proc-name (gen-proc-name ch-name proc idx)))
			 (setf (name proc) proc-name)
			 (setf (gethash proc-name *processor-directory*) proc)))
		      ((or (typep proc 'graph-event-processor)
			   (typep proc 'mpfa-event-processor))
		       proc)
		      ))
	    proc-list)))

(defmacro chain (name (&key (activate nil) (shift 0.0) (group nil))
		 &body proc-body)
  `(funcall #'(lambda ()
		(let ((event-processors
		       (gen-proc-list ,name (list ,@proc-body))))
		(chain-from-list
		 ,name
		 event-processors		 
		 :activate ,activate
		 :shift ,shift
		 :group ,group)))))

;; if no group is given, the current group will be used ... 
(defun assign-chain-to-group (chain chain-name group)
  ;; if no groupname is given, use current group ... 
  (let* ((groupname (if group group *current-group*))
	 (group-list (gethash groupname *group-directory*)))
    (when (not (member chain-name group-list))
      (setf (chain-group chain) group)
      (setf (gethash groupname *group-directory*)
	      (append group-list (list chain-name))))))

(defmethod collect-chain ((c processor-chain) &key)
  (labels ((append-next (proc-list proc)	     
	     (if (successor proc)
		 (append-next (append proc-list (list proc))  (successor proc))
		 (append proc-list (list proc)))))
    (append-next '() (topmost-processor c))))

;; to change - push branch to regular chain directory,
;; just store the name in branch list.
;; that should allow for independent growth of branches ! 
(defun chain-from-list (name event-processors &key (activate nil)
						(shift 0.0)
						(branch nil)
						(group nil))  
  (connect event-processors name)
  ;; assume the chaining went well 
  (let ((topmost-proc (car event-processors)))
    (if (chain-bound topmost-proc)
	(let ((new-chain (make-instance
			  'processor-chain
			  :topmost topmost-proc
			  :is-active activate
			  :shift shift
                          :name name)))	  
	  ;; assign chain to a group
	  (assign-chain-to-group new-chain name group)
	  ;; handle branching ...	  
	  (if branch
	      (setf (gethash branch *branch-directory*)
		    (append (gethash branch *branch-directory*) (list name))))
	  (setf (gethash name *chain-directory*) new-chain))
	(incudine::msg error "chain-building went wrong, seemingly ..."))))





