(in-package :megra)

(defclass clock-sync ()
  ((synced-chains :accessor clock-sync-synced-chains :initform nil)
   (osc-input-stream :accessor clock-sync-osc-input-stream :initform nil :initarg :stream)))

(defun register-clock-sync (name address &key (port 12350) (sig "") (host "127.0.0.1"))  
  (let* ((istr (osc:open :host host :port port))
	 (cs (make-instance 'clock-sync :stream istr)))
    (incudine::recv-start istr)
    (incudine::make-osc-responder istr address ""
				  (lambda ()
				    (when (clock-sync-synced-chains cs)
				      (loop for synced-chain in (clock-sync-synced-chains cs)	     
					 ;; don't check if it's active, as only deactivated procs
					 ;; are added to sync list
					 do (let ((sync-shift (chain-shift synced-chain)))	        
					      (activate synced-chain)	      
					      (setf (chain-shift synced-chain) 0)
					      ;; secure this to ensure smooth operation in case of
					      ;; forgotten graphs ... 	        
					      (handler-case
						  (incudine:aat (+ (incudine:now) #[(chain-shift synced-chain) ms])
								#'perform-dispatch
								synced-chain				     
								it)		  		
						(simple-error (e) (incudine::msg error "~D" e)))))
				      ;; reset all synced processors
				      (setf (clock-sync-synced-chains cs) nil))))
    (setf (gethash name *clock-directory*) cs)))






