(in-package :megra)

;; helper ...
(defun radians (numberOfDegrees) 
  (* pi (/ numberOfDegrees 180.0)))

;; knuth shuffle, needed as helper ...
(defun shuffle-list (l)
  (loop for i from (- (list-length l) 1) downto 1
        do (let* ((current-elem-idx (random i))
	          (random-elem (nth current-elem-idx l)))	  
	     (setf (nth current-elem-idx l) (nth i l))
	     (setf (nth i l) random-elem)))
  ;; return shuffled list ... somewhat imperative, again .. 
  l)

(defun clear-all ()
  ;; first of all stop all events already passed to incudine ...
  (incudine::flush-pending)
  (setf *processor-directory* (make-hash-table :test 'eql))
  (loop for chain being the hash-values of *global-syncs*
     do (deactivate chain))       
  (setf *global-syncs* (make-hash-table :test 'eql))  
  (setf *multiglobal-syncs* (make-hash-table :test 'eql)))

(defun clear-single (id)
  ;; if it's a chain, stop the chain ...  
  (incudine::msg error "clear ~D" id)
  (stop id)
  (remhash id *global-syncs*))

(defun clear (&rest chains)
  (if (<= (length chains) 0)
      (clear-all)
      (mapc #'clear-single chains)))

(defun stop (&rest chains)
  "stop a chain or (if no argument given) everything"
  (if (<= (length chains) 0)
      (loop for chain being the hash-values of *global-syncs*
	 do (deactivate chain))
      (mapc #'(lambda (id)
		(incudine::msg error "stop ~D" id)
		;; if it's a chain, stop the chain ...
		(deactivate (gethash id *global-syncs*)))
	    chains)))

;; convenience functions to set params in some object ...
(defun pset (object param value)
  (setf (slot-value (gethash object *processor-directory*) param) value))

(defmacro sync-progn (ch &body funcs)
  `(funcall #'(lambda ()
		(let ((chain (gethash ,ch *global-syncs*)))
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
  (loop for ch being the hash-values of *global-syncs*
     do (when (is-active ch)
	  (format t "~D " (name ch))))
  (format t "~%"))

;; compose function allows for convenient application
;; of higher-order functions ...
(defun cmp (&rest rest)
  (if (cdr rest)
      (let ((rev (reverse rest)))
        (labels ((accum (acc r)
                   (if r
                       (accum (funcall (car r) acc) (cdr r))
                       acc)))
          (accum (funcall (cadr rev) (car rev)) (cddr rev))))      
      (car rest)))

       
       
  
