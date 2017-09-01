;; event dispatching and related stuff ... 

;; simple time-recursive dispatching
;; not using local variable binding to reduce consing (??)
(in-package :megra)

(defun perform-dispatch (chain-id osc-time incudine-time)
  (let ((chain (gethash chain-id *chain-directory*)))    
    (when (and chain (is-active chain))
      ;; here, the events are produced and handled ...
      (when (synced-chains chain)
	(loop for synced-chain in (synced-chains chain)
	   ;; don't check if it's active, as only deactivated procs
	   ;; are added to sync list
	   do (progn
		(activate synced-chain)
		(perform-dispatch synced-chain osc-time incudine-time)))
	;; reset all synced processors
	(setf (synced-chains chain) nil))
      ;; handle events from current graph
      (handle-events (pull-events chain) osc-time)
      ;; here, the transition time between events is determinend,
      ;; and the next evaluation is scheduled ...
      ;; this method works only with SC,
      ;; with INCUDINE itself it'll be imprecise ... 
      (let* ((trans-time (transition-duration (car (pull-transition chain))))
	     (next-osc-time (+ osc-time (* trans-time 0.001)))
	     (next-incu-time (+ incudine-time
				#[(- next-osc-time (incudine::timestamp)) s])))
	(incudine:aat next-incu-time
		      #'perform-dispatch chain-id next-osc-time it)))))

(defun perform-dispatch-norepeat (proc time)
  (let ((event-processor (gethash proc *processor-directory*)))    
    (when (and event-processor (is-active event-processor))
      ;; here, the events are produced and handled ...
      (when (synced-processors event-processor)
	(loop for synced-proc in (synced-processors event-processor)
	   ;; don't check if it's active, as only deactivated procs are added to sync list
	   do (progn
		(activate synced-proc)
		(perform-dispatch synced-proc (incudine:now))))
	;; reset all synced processors
	(setf (synced-processors event-processor) nil))     
      ;; handle events from current graph
      (handle-events (pull-events event-processor))
      ;; here, the transition time between events is determinend,
      ;; and the next evaluation is scheduled ...
      (let* ((trans-time (transition-duration (car (pull-transition event-processor))))
	     (next (+ time #[trans-time ms])))))))

(defun handle-events (events osc-timestamp)
  (mapc #'(lambda (event) (handle-event event (+ osc-timestamp *global-osc-delay*))) events))

(in-package :megra)
;; if 'unique' is t, an event processor can only be hooked into one chain.
(defmacro dispatch (name (&key (sync-to nil) (unique t)) &body proc-body)
  `(funcall #'(lambda ()
		(let ((event-processors (list ,@proc-body))
		      (old-chain (gethash ,name *chain-directory*)))		      		     
		  ;; first, construct the chain ...
		  (cond ((and old-chain (>= 0 (length event-processors)))
			 ;; if chain is active, do nothing, otherwise activate 
			 (incudine::msg info "chain ~D already present, handling it ..." ,name))			    
			((and old-chain (< 0 (length event-processors)))
			 (incudine::msg info "chain ~D already present (active: ~D), rebuilding it ..." ,name (is-active old-chain))
			 ;; rebuild chain, activate
			 (unless (chain ,name (:activate (is-active old-chain)) ,@proc-body)
			   (incudine::msg error "couldn't rebuild chain ~D, active: ~D" ,name (is-active old-chain))))	       	     
			((>= 0 (length event-processors))
			 ;; if there's no chain present under this name, and no material to build one,
			 ;; it's an error condition ...
			 (incudine::msg error "cannot build chain ~D from nothing ..." ,name))
			((< 0 (length event-processors))
			 (incudine::msg info "new chain ~D, trying to build it ..." ,name)
			 ;; build chain, activate
			 (unless (chain ,name () ,@proc-body)
			   (incudine::msg error "couldn't build chain ~D" ,name)))
			(t (incudine::msg error "invalid state"))))
		(incudine::msg info "hopefully built chain ~D ..." ,name)
		;; if we've reached this point, we should have a valid chain, or left the function ...
		(let ((chain (gethash ,name *chain-directory*)))
		  (if (and ,sync-to (gethash ,sync-to *chain-directory*))
		      (progn			 
			(deactivate ,name)
			(unless (member ,name (synced-chains (gethash ,sync-to *chain-directory*)))
			  (incudine::msg info "syncing ~D to ~D, ~D will start at next dispatch of ~D" ,name ,sync-to ,name ,sync-to)
			  (setf (synced-chains (gethash ,sync-to *chain-directory*))
				(append (synced-chains (gethash ,sync-to *chain-directory*))
					(list ,name)))))
		      (unless (is-active chain)
			(activate ,name)
			(incudine:at (incudine:now) #'perform-dispatch
				     ,name
				     (incudine:timestamp) (incudine:now))))))))

;; "sink" alias for "dispatch" ... shorter and maybe more intuitive ... 
(setf (macro-function 'sink) (macro-function 'dispatch))

