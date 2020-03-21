(in-package :megra)

(defclass processor-chain (event-processor)
  ((sync-id :accessor sync-id :initarg :sync-id)
   (topmost-processor :accessor topmost-processor :initarg :topmost)
   (synced-chains :accessor synced-chains :initform nil)
   (synced-progns :accessor synced-progns :initform nil)
   ;; think of anschluss-zug -> connection train ... 
   (anschluss-kette :accessor anschluss-kette :initform nil) 
   (wait-for-sync :accessor wait-for-sync :initform nil)
   (active :accessor is-active :initform nil :initarg :is-active)
   (shift :accessor chain-shift :initform 0.0 :initarg :shift)))

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
		      ((and (not (typep proc 'generator))			    
			    (not (gethash (name proc) *processor-directory*)))
		       (let ((proc-name (gen-proc-name ch-name proc idx)))
			 (setf (name proc) proc-name)
			 (setf (gethash proc-name *processor-directory*) proc)))
		      ((typep proc 'generator)
		       proc)))
	    (remove nil proc-list))))

(defmacro chain (name (&key (activate nil) (shift 0.0)) &body proc-body)
  `(funcall #'(lambda ()
		(let ((event-processors
		        (gen-proc-list ,name (list ,@proc-body))))                  
		  (chain-from-list
		   ,name
		   event-processors		 
		   :activate ,activate
		   :shift ,shift)))))

(defmethod collect-chain ((c processor-chain) &key)
  (labels ((append-next (proc-list proc)	     
	     (if (successor proc)
		 (append-next (append proc-list (list proc))  (successor proc))
		 (append proc-list (list proc)))))
    (append-next '() (topmost-processor c))))

(defun chain-from-list (name event-processors &key (activate nil) (shift 0.0))  
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
	  (setf (gethash name *chain-directory*) new-chain))
	(incudine::msg error "chain-building went wrong, seemingly ..."))))
