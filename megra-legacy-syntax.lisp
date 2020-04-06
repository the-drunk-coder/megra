(in-package :megra)

;; a few functions to ensure some compatibility with older megra code ..


;; old dispatch function ...
(defmacro dispatch-old (name (&key (sync nil) (branch nil) (shift 0.0) (intro nil)) &body proc-body)
  `(funcall (lambda ()
              (sx ',name t :sync ',sync :shift ,shift :intro ,intro
                  (cmp ,@proc-body)))))

;; "sink" alias for "dispatch" ... shorter and maybe more intuitive ... 
(setf (macro-function 'sink) (macro-function 'dispatch-old))

;; even shorter, tidal style ... 
(setf (macro-function 's) (macro-function 'dispatch-old))

;; old node
(defun node (id &rest content) (list 'node id content))

;; shorthand for old node 
(defun n (id &rest content) (list 'node id content))

(defun edge (src dest &key prob (dur 512)) (list 'edge src dest prob dur))

;; shorthand for edge
(defun e (src dest &key p (d 512)) (list 'edge src dest p d))

;; old graph syntax 
(defmacro graph (name (&key		       
		         (combine-mode ''append)
		         (affect-transition nil)
		         (combine-filter #'all-p)		         
		         (rand 0))
		 &body graphdata)
  `(funcall (lambda ()
              (infer-from-rules-fun :type 'naive
                                    :name ,name
                                    :mapping (p-events-list (loop for i in (list ,@graphdata)
                                                                  when (equal (car i) 'node)
                                                                  nconc (cdr i)))
	                            :rules (loop for i in (list ,@graphdata)
                                                 when (equal (car i) 'edge)
                                                 collect (nconc (list (list (cadr i))) (cddr i)))))))

(defun lifemodel (act growth-cycle lifespan &rest rest)
  (let ((variance (find-keyword-val :var rest :default 0.2)))
    (if act
        (apply 'life growth-cycle lifespan variance rest)
        (alexandria::lastcar rest))))

(defun lm (act growth-cycle lifespan &rest rest)
  (let ((variance (find-keyword-val :var rest :default 0.2)))
    (if act
        (apply 'life growth-cycle lifespan variance rest)
        (alexandria::lastcar rest))))


