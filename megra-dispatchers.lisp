;; event dispatching and related stuff ... 

;; simple time-recursive dispatching
(in-package :megra)

(defun perform-dispatch-sep-times (chain osc-time incudine-time)
  ;; create anschluss when old instance has been deactivated (hopefully)
  (when (anschluss-kette chain)    
    (let ((sync-shift (chain-shift (anschluss-kette chain))))
      (handler-case		    
	  (incudine:aat (+ incudine-time #[sync-shift ms])
			#'perform-dispatch-sep-times
			(anschluss-kette chain)    
			(+ osc-time (* sync-shift 0.001))
			it)		  		
	(simple-error (e) (incudine::msg error "~D" e)))))
  ;; regular case ... 
  (when (and chain (is-active chain))
    ;; here, the events are produced and handled ...
    (when (synced-chains chain)
      (loop for synced-chain in (synced-chains chain)	     
	 ;; don't check if it's active, as only deactivated procs
	 ;; are added to sync list
	 do (let ((sync-shift (chain-shift synced-chain)))	        
	      (activate synced-chain)
	      (setf (wait-for-sync synced-chain) nil)
	      (setf (chain-shift synced-chain) 0)
	      ;; secure this to ensure smooth operation in case of
	      ;; forgotten graphs ... 	        
	      (handler-case		    
		  (incudine:aat (+ incudine-time #[sync-shift ms])
				#'perform-dispatch-sep-times
				synced-chain
				(+ osc-time (* sync-shift 0.001))
				it)		  		
		(simple-error (e) (incudine::msg error "~D" e)))))
      ;; reset all synced processors
      (setf (synced-chains chain) nil))
    (handler-case
	(when (synced-progns chain)
	  (mapc #'funcall (synced-progns chain))
	  (setf (synced-progns chain) nil))
      (simple-error (e)
	(incudine::msg error "cannot handle sync-progns: ~D" e)
	(setf (synced-progns chain) nil)))
    ;; handle events from current graph
    ;; again, secure this, so that the chain can be restarted
    ;; without having to clear everything ...
    (handler-case (handle-events (pull-events chain) osc-time)
      (simple-error (e)
	(incudine::msg error "cannot pull and handle events: ~D" e)
	;;(setf (is-active chain) nil)
	))
    
    ;; here, the transition time between events is determinend,
    ;; and the next evaluation is scheduled ...
    ;; this method works only with SC,
    ;; with INCUDINE itself it'll be imprecise ... 
    (let* ((trans-time (transition-duration (car (pull-transition chain))))
	   (next-osc-time (+ osc-time (* trans-time 0.001)))
	   (next-incu-time (+ incudine-time
			      #[(- next-osc-time (incudine::timestamp)) s])))
      (incudine:aat next-incu-time
		    #'perform-dispatch chain next-osc-time it))))

(defun perform-dispatch (chain incudine-time)
  ;; create anschluss when old instance has been deactivated (hopefully)
  (when (anschluss-kette chain)    
    (let ((sync-shift (chain-shift (anschluss-kette chain))))
      (handler-case		    
	  (incudine:aat (+ incudine-time #[sync-shift ms])
			#'perform-dispatch
			(anschluss-kette chain)    		        
			it)		  		
	(simple-error (e) (incudine::msg error "~D" e)))))
  ;; regular case ... 
  (when (and chain (is-active chain))
    ;; here, the events are produced and handled ...
    (when (synced-chains chain)
      (loop for synced-chain in (synced-chains chain)	     
	 ;; don't check if it's active, as only deactivated procs
	 ;; are added to sync list
	 do (let ((sync-shift (chain-shift synced-chain)))	        
	      (activate synced-chain)
	      (setf (wait-for-sync synced-chain) nil)
	      (setf (chain-shift synced-chain) 0)
	      ;; secure this to ensure smooth operation in case of
	      ;; forgotten graphs ... 	        
	      (handler-case		    
		  (incudine:aat (+ incudine-time #[sync-shift ms])
				#'perform-dispatch
			        synced-chain
				it)		  		
		(simple-error (e) (incudine::msg error "~D" e)))))
      ;; reset all synced processors
      (setf (synced-chains chain) nil))
    (handler-case
	(when (synced-progns chain)
	  (mapc #'funcall (synced-progns chain))
	  (setf (synced-progns chain) nil))
      (simple-error (e)
	(incudine::msg error "cannot handle sync-progns: ~D" e)
	(setf (synced-progns chain) nil)))
    ;; handle events from current graph
    ;; again, secure this, so that the chain can be restarted
    ;; without having to clear everything ...
    (handler-case (handle-events (pull-events chain) (incudine::rt-time-offset))
      (simple-error (e)
	(incudine::msg error "cannot pull and handle events: ~D" e)))    
    ;; here, the transition time between events is determinend,
    ;; and the next evaluation is scheduled ...    
    (let* ((trans-time (transition-duration (car (pull-transition chain))))	   
	   (next-incu-time (+ incudine-time #[trans-time ms])))
      (incudine:aat next-incu-time #'perform-dispatch chain it))))

(defun handle-events (events osc-timestamp)
  (mapc #'(lambda (event) (handle-event event (+ osc-timestamp *global-osc-delay*))) events))

;; if 'unique' is t, an event processor can only be hooked into one chain.
;; somehow re-introduce msync ? unique is basically msync without sync ... 
;; as of november 2017, i don't even rememeber what i ever meant by it ... 
(defmacro dispatch (name (&key (sync-to nil) (branch nil) (group nil) (unique t) (shift 0.0)) &body proc-body)
  ;; when we're branching the chain, we temporarily save the state of all processor
  ;; directories (as we cannot be sure which ones are used ...)
  `(funcall #'(lambda ()
		;; copy current state to make branching possible ...
		(when ,branch
		  (incudine:nrt-funcall  
		   (loop for proc-id being the hash-keys of *processor-directory*
		      do (setf (gethash proc-id *prev-processor-directory*)
			       (clone proc-id proc-id :track nil :store nil)))))
		(let* ((event-processors
			;; replace symbols by instances, generate proper names, insert into proc directory
			(gen-proc-list ,name (list ,@proc-body)))
		       (old-chain (gethash ,name *chain-directory*)))
		  ;; first, construct the chain ...
		  (cond ((and ,branch old-chain)
			 ;; if we're branching, move the current chain to the branch directory
			 ;; and replace the one in the chain-directory by a copy ...
			 (incudine::msg info "branching chain ~D" ,name)			 
			 (let* ((shift-diff (max 0 (- ,shift (chain-shift old-chain))))
				;; build a chain from the previous states of the event processors ...				
				(real-old-chain (chain-from-list ,name
								 (mapcar #'(lambda (proc)									
									     (gethash (name proc) *prev-processor-directory*))
									 event-processors)
								 :activate (is-active old-chain)
								 :shift shift-diff
								 :group ,group))
				;; build the new chain from the current states 
				(new-chain (chain-from-list ,name
							    (mapcar #'(lambda (proc)									
									(clone (name proc) (gensym (symbol-name (name proc))) :track nil))
								    event-processors)
							    :activate nil
							    :shift shift-diff
							    :group ,group)))
			   (if (not new-chain)
			       (incudine::msg error "couldn't rebuild chain ~D, active: ~D" ,name (is-active old-chain)))
			   ;; in that case, the syncing chain will do the
			   (deactivate old-chain) ;; dactivate old chain and set anschluss
			   (setf (anschluss-kette old-chain) real-old-chain)
			   (setf (gethash ,name *branch-directory*) (append (gethash ,name *branch-directory*) (list real-old-chain)))))
			((and old-chain (wait-for-sync old-chain))			 
			 (incudine::msg info "chain ~D waiting for sync ..." ,name))
			((and old-chain (>= 0 (length event-processors)))
			 ;; this (probably) means that the chain has been constructed by the chain macro
			 ;; OR that the chain would be faulty and thus, was not built (i.e. if it contained a proc
			 ;; that doesn't exist)		     
			 ;; if chain is active, do nothing, otherwise activate
			 (setf (chain-shift old-chain) (max 0 (- ,shift (chain-shift old-chain))))
			 ;; assign to group in case chain macro hasn't done this ...
			 (when ,group (assign-chain-to-group ,name ,group))
			 (incudine::msg info "chain ~D already present (maybe the attempt to rebuild was faulty ?), handling it ..." ,name))			    
			((and old-chain (< 0 (length event-processors)))
			 ;; this means that the chain will be replaced ... 
			 (incudine::msg info "chain ~D already present (active: ~D), rebuilding it ..." ,name (is-active old-chain))
			 ;; rebuild chain, activate, create "anschluss" to old chain (means s.th. flange or continuity)
			 (let* ((shift-diff (max 0 (- ,shift (chain-shift old-chain))))
				(new-chain (chain-from-list ,name event-processors :activate (is-active old-chain) :shift shift-diff :group ,group)))
			   (if (not new-chain)
			       (incudine::msg error "couldn't rebuild chain ~D, active: ~D" ,name (is-active old-chain)))
			   ;; in that case, the syncing chain will do the anschluss ...
			   (unless (gethash ,sync-to *chain-directory*) (setf (anschluss-kette old-chain) new-chain))
			   (deactivate old-chain))) 
			((>= 0 (length event-processors))
			 ;; if there's no chain present under this name, and no material to build one,
			 ;; it's an error condition ...
			 (incudine::msg error "cannot build chain ~D from nothing" ,name))
			((< 0 (length event-processors))
			 (incudine::msg info "new chain ~D, trying to build it ..." ,name)
			 ;; build chain, activate
			 (unless (chain-from-list ,name event-processors :shift ,shift :group ,group)
			   (incudine::msg error "couldn't build chain ~D" ,name)))
			(t (incudine::msg error "invalid state"))))
		(incudine::msg info "hopefully built chain ~D ..." ,name)
		;; if we've reached this point, we should have a valid chain, or left the function ...
		(let ((chain (gethash ,name *chain-directory*))
		      (chain-to-sync-to (gethash ,sync-to *chain-directory*)))
		  ;; now, if we want to sync the current chain to :sync-to,
		  ;; and :sync-to denotes a chain that is actually present,
		  (if chain-to-sync-to
		      ;; when the current chain is NOT yet synced to chain-to-sync-to ...		      
		      (unless (wait-for-sync chain)
			(deactivate chain)
			(setf (wait-for-sync chain) t)
			(incudine::msg info "syncing ~D to ~D, ~D will start at next dispatch of ~D" ,name ,sync-to ,name ,sync-to)
			(setf (synced-chains chain-to-sync-to)
			      (append (synced-chains chain-to-sync-to)
				      (list chain))))		      
		      (unless (or (is-active chain) (wait-for-sync chain))					        
			(activate chain)
			;;(incudine:at (+ (incudine:now) #[(chain-shift chain) ms])
			;;	     #'perform-dispatch-sep-times
			;;	     chain
			;;	     (+ (incudine:timestamp) (* (chain-shift chain) 0.001))
			;;	     (+ (incudine:now) #[(chain-shift chain) ms]))
			(incudine:aat (+ (incudine:now) #[(chain-shift chain) ms])
				      #'perform-dispatch
				      chain				     
				      it)))))))

;; "sink" alias for "dispatch" ... shorter and maybe more intuitive ... 
(setf (macro-function 'sink) (macro-function 'dispatch))
;; even shorter, tidal style ... 
(setf (macro-function 's) (macro-function 'dispatch))

