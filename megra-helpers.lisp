(in-package :megra)

(defun clear-all ()
  ;; first of all stop all events already passed to incudine ...
  (incudine::flush-pending)
  (setf *processor-directory* (make-hash-table :test 'eql))
  (loop for chain being the hash-values of *chain-directory*
     do (deactivate chain))       
  ;;(loop for branch being the hash-values of *branch-directory*
  ;;   do (mapc #'deactivate branch))       
  (setf *chain-directory* (make-hash-table :test 'eql))
  (setf *group-directory* (make-hash-table :test 'eql))
  (setf *branch-directory* (make-hash-table :test 'eql))
  (setf *current-group* 'DEFAULT))

(defun clear-single (id)
  (cutall id)
  ;; if it's a group, stop the group
  (if (gethash id *group-directory*)
      (mapc #'(lambda (chain)
		(stop chain)
		(remhash chain *chain-directory*))
	    (gethash id *group-directory*))
      ;; if it's a chain, stop the chain ...
      (progn
	(stop id)
	(remhash id *chain-directory*)
	(remhash id *branch-directory*))))

(defun clear (&rest chains)
  (if (<= (length chains) 0)
      (clear-all)
      (mapc #'clear-single chains)))

(defun cutall (chain-or-group-id)
  "cut all branches"
  (if (gethash chain-or-group-id *group-directory*)
      (mapc #'cutall (gethash chain-or-group-id *group-directory*))  
      (progn
	(mapc #'(lambda (id) (deactivate (gethash id *chain-directory*)))
	      (gethash chain-or-group-id *branch-directory*))
	(setf (gethash chain-or-group-id *branch-directory*) nil))))

(defun cut (chain-id)
  "cut the latest branch"
  (let* ((branches (gethash chain-id *branch-directory*))
	 (last (car (reverse branches))))
    (deactivate (gethash last *chain-directory*))
    (setf (gethash chain-id *branch-directory*) (delete last branches))))

(defun stop (&rest chains)
  "stop a chain or (if no argument given) everything"
  (if (<= (length chains) 0)
      (loop for chain being the hash-values of *chain-directory*
	 do (deactivate chain))
      (mapc #'(lambda (id)
		(cutall id)
		;; if it's a group, stop the group
		(if (gethash id *group-directory*)
		    (mapc #'(lambda (chain)
			      (deactivate (gethash chain *chain-directory*)))
			  (gethash id *group-directory*))
		    ;; if it's a chain, stop the chain ...
		    (deactivate (gethash id *chain-directory*))))
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

;; set the default group
(defun group (groupname)
  (setf *current-group* groupname))

(defmacro e- (&body li) `(funcall #'(lambda () (list ,@li))))
