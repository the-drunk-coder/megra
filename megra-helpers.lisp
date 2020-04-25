(in-package :megra)

(defun probability-list-hash-table (seq)
  (let ((key)
        (events (make-hash-table)))
    (loop for item in seq
          when (or (typep item 'param-mod-object) (numberp item))
          do (setf key item)
          and do (setf (gethash key events) (list))
          when (typep item 'event)
          do (setf (gethash key events) (nconc (gethash key events) (list item)) ))
    events))

;; helper functions for shorthands ...
(defun find-keyword-list (keyword seq)
  (when (and
         (member keyword seq)
         (> (length (member keyword seq)) 0) ;; check if there's chance the keyword has a value ...
         (not (eql (type-of (cadr (member keyword seq))) 'keyword)))
    (let* ((pos (position keyword seq))
	   (vals (loop for val in (cdr (member keyword seq))
                       while (not (keywordp val))
                       collect val)))
      vals)))

;; helper functions for shorthands ...
(defun find-keyword-symbol-list (keyword seq)
  (when (and
         (member keyword seq)
         (> (length (member keyword seq)) 0) ;; check if there's chance the keyword has a value ...
         (symbolp (cadr (member keyword seq))))
    (let* ((pos (position keyword seq))
	   (vals (loop for val in (cdr (member keyword seq))
                       while (symbolp val)
                       collect val)))
      vals)))

(defun find-keyword-val (keyword seq &key default)
  (if (and
       (member keyword seq)
       (> (length (member keyword seq)) 0) ;; check if there's chance the keyword has a value ...
       (not (eql (type-of (cadr (member keyword seq))) 'keyword)))
      (let* ((pos (position keyword seq))
	     (val (nth (+ pos 1) seq)))
	val)
      default))

(defun p-events-list (event-plist)  
  (let ((mapping (make-hash-table :test #'equal))
	(key))    
    (loop for m in event-plist 
	  do (if (or (typep m 'symbol) (typep m 'number))
 		 (progn
		   (setf key m)
		   (setf (gethash key mapping) (list)))
		 
                 (if (typep m 'list)
                     (loop for ev in m do (push ev (gethash key mapping)))
                     (push m (gethash key mapping)))))
    mapping))

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
  (if *vis-active* (loop for g being the hash-value of *processor-directory*
                         do (vis-clear g)))
  (setf *processor-directory* (make-hash-table :test 'eql))
  (loop for chain being the hash-values of *global-syncs*
     do (deactivate chain))       
  (setf *global-syncs* (make-hash-table :test 'eql))  
  (setf *multichain-directory* (make-hash-table :test 'eql)))

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
                (if (gethash id *global-syncs*)
                    (stop-sync (gethash id *global-syncs*))
                    (mapc #'(lambda (id2) (stop-sync (gethash id2 *global-syncs*))) (gethash id *multichain-directory*))))
	    chains)))

(defun stop-sync (sync)
  (deactivate sync)
  (if *vis-active* (vis-clear (processor sync))))

(defun solo (&rest chains)
  "solo one or more chain"
  (loop for chain in chains
        when (gethash chain *multichain-directory*)
        do (loop for multichain being the hash-keys of *multichain-directory*
                 when (not (member multichain chains))
                 do (loop for dchain in (gethash multichain *multichain-directory*)
                          do (deactivate (gethash dchain *global-syncs*))))
        when (gethash chain *global-syncs*)
        do (loop for ichain being the hash-keys of *global-syncs*
                 when (not (member ichain chains))
                 do (format t "stop ~D~%" ichain)
                 and do (deactivate (gethash ichain *global-syncs*)))))

(defun getgen (name)
  (gethash name *processor-directory*))

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

(defun multi-filter (filters)
  (lambda (event)
    (> (loop for f in filters summing (if (member f (event-tags event)) 1 0)) 0)))

(defun multi-filter-not (filters)
  (lambda (event)
    (> (loop for f in filters summing (if  (member f (event-tags event)) 0 1)) 0)))

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

       
       
  
