(in-package :megra)

(defun clear-all ()
  ;; first of all stop all events already passed to incudine ...
  (incudine::flush-pending)
  (setf *processor-directory* (make-hash-table :test 'eql))
  (loop for chain being the hash-values of *chain-directory*
     do (deactivate chain))       
  (setf *chain-directory* (make-hash-table :test 'eql))  
  (setf *multichain-directory* (make-hash-table :test 'eql)))

(defun clear-single (id)
  ;; if it's a chain, stop the chain ...  
  (incudine::msg error "clear ~D" id)
  (stop id)
  (remhash id *chain-directory*))

(defun clear (&rest chains)
  (if (<= (length chains) 0)
      (clear-all)
      (mapc #'clear-single chains)))

(defun stop (&rest chains)
  "stop a chain or (if no argument given) everything"
  (if (<= (length chains) 0)
      (loop for chain being the hash-values of *chain-directory*
	 do (deactivate chain))
      (mapc #'(lambda (id)
		(incudine::msg error "stop ~D" id)
		;; if it's a chain, stop the chain ...
		(deactivate (gethash id *chain-directory*)))
	    chains)))

;; convenience functions to set params in some object ...
(defun pset (object param value)
  (setf (slot-value (gethash object *processor-directory*) param) value))

(defmacro sync-progn (ch &body funcs)
  `(funcall #'(lambda ()
		(let ((chain (gethash ,ch *chain-directory*)))
		  (when chain		    
		    (setf (synced-progns chain)
			  (append (synced-progns chain)
				  (list (lambda () ,@funcs)))))))))

(defmacro ~ (&body li) `(funcall #'(lambda () (list ,@li))))

(defmacro % (&body li) `(funcall #'(lambda () (list ,@li))))

(defmacro define-filter (tag)
  (let ((name-proc (concatenate 'string (symbol-name tag) "-p")))
    `(funcall (lambda ()	      
		(defun ,(read-from-string name-proc) (event)
		  (member ',tag (event-tags event)))))))


(defun mon ()
  (format t "ACTIVE CHAINS: ")
  (loop for ch being the hash-values of *chain-directory*
     do (when (is-active ch)
	  (format t "~D " (name ch))))
  (format t "~%"))
	  
       
       
  
