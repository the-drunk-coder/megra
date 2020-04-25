(in-package :megra)

(defparameter *label-map* (make-hash-table :test 'equal))
(defparameter *vis-active* nil)

(defun vis-start-client ()
  (defvar *oscout-vis* (osc:open :host "127.0.0.1" :port 57121 :direction :output :latency 0))
  (setf *vis-active* t))

(defmethod vis-update-create ((g generator) &key)
  ;; create graph in vis server
  (osc:message *oscout-vis* "/graph/add" "s" (symbol-name (generator-name g)))
  ;; keep labels so we don't have to generate them all the time ...  
  (setf (gethash (generator-name g) *label-map*) (make-hash-table :test 'equal))
  (let ((the-labels (gethash (generator-name g) *label-map*)))
    ;; add nodes
    (loop for n being the hash-keys of (vom::children (inner-generator g)) using (hash-value chs)
          do (let* ((key (sxhash n))
                    (label (if (gethash key the-labels)
                               (gethash key the-labels)
                               (with-output-to-string (stream)                                
                                 (loop for s in n
                                       do (format stream "~D "
                                                  (let ((lab (alexandria::lastcar (event-tags (car (gethash s (event-dictionary g)))))))
                                                    (if (equal lab 'silence) '~ lab))))))))
               ;; store for posterity
               (if (not (gethash key the-labels)) (setf (gethash key the-labels) label))               
               ;; add node
               (osc:message *oscout-vis* "/node/add" "sis" (symbol-name (generator-name g)) key label)
               (loop for ch in chs
                     do (let* ((dest (if (listp (cdr ch)) (cadr ch) (list (cdr ch))))
                               (dest-key (sxhash dest)))                        
                          (osc:message *oscout-vis* "/edge/add" "siisi"
                                       (symbol-name (generator-name g))
                                       key
                                       dest-key
                                       (symbol-name (alexandria::lastcar (event-tags (car (gethash (alexandria::lastcar dest) (event-dictionary g))))))
                                       (car ch)))))))
  (osc:message *oscout-vis* "/render" "s" (symbol-name (generator-name g))))


(defmethod vis-update-active-node ((g generator) &key)
  (let* ((key (sxhash (vom::query-result-last-state (last-transition g))))
         (label (gethash key (gethash (generator-name g) *label-map*))))    
    (if (and key label)
        (osc:message *oscout-vis* "/node/active" "sis" (symbol-name (generator-name g)) key label))))

(defmethod vis-update ((g generator) &key)
  (if (is-modified g)
      (vis-update-create g)
      (setf (is-modified g) nil))
  (if (and (is-active g) (vom::query-result-last-state (last-transition g)))
      (vis-update-active-node g))
  (if (successor g)
      (vis-update (successor g))))

(defmethod vis-update ((w event-processor-wrapper) &key)
  (vis-update (wrapper-wrapped-processor w)))

(defmethod vis-clear ((g generator) &key)
  (osc:message *oscout-vis* "/clear" "s" (symbol-name (generator-name g))))

(defmethod vis-clear ((w event-processor-wrapper) &key)
  (vis-clear (wrapper-wrapped-processor w)))



  
  
