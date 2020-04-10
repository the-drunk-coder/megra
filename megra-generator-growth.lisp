(in-package :megra)

(defmethod grow-generator (graph-or-id &key (var 0)		        
			                    durs
			                    functors
			                    (method 'old)
			                    (rnd 0)
			                    (higher-order 0))
  (let* ((g (if (typep graph-or-id 'symbol)
		(gethash graph-or-id *processor-directory*)
		graph-or-id))
         (result (cond ((eql method 'triloop)
	                (vom::grow-triloop (inner-generator g) 
	                                   :rnd rnd
	                                   :higher-order higher-order))
	               ((eql method 'quadloop)
	                (vom::grow-quadloop (inner-generator g)
				            :rnd rnd
				            :higher-order higher-order))
	               ((eql method 'loop)
	                (vom::grow-loop (inner-generator g)
			                :rnd rnd
			                :higher-order higher-order))
                       ((eql method 'flower)
	                (vom::grow-flower (inner-generator g)
			                  :rnd rnd
			                  :higher-order higher-order))
	               (t (vom::grow-old (inner-generator g)
		                         :rnd rnd
		                         :higher-order higher-order)))))
    ;; set the new event ...
    (setf (gethash (vom::growth-result-added-symbol result) (event-dictionary g))
          (deepcopy-list (gethash (vom::growth-result-template-symbol result) (event-dictionary g))
			 :imprecision var
			 :functors functors))
    ;; set symbol age to zero ...
    (setf (gethash (vom::growth-result-added-symbol result) (ages g)) 0)
    ;; now for the durations ...
    ;; could be more sophisticated ...
    (let ((appropiate-duration            
            (cond (durs (alexandria::random-elt durs))
                  ((vom::growth-result-removed-transitions result) (gethash (car (vom::growth-result-removed-transitions result)) (transition-durations g))))))
      (if appropiate-duration
          (loop for added in (vom::growth-result-added-transitions result)
                do (setf (gethash added (transition-durations g)) appropiate-duration)))
      (list result appropiate-duration))))

(defmethod prune-generator (graph-or-id &key exclude node-id)
  (let ((g (if (typep graph-or-id 'symbol)
	       (gethash graph-or-id *processor-directory*)
	       graph-or-id)))
    ;; remove symbol from ages !!!
    (vom::prune-pfa (inner-generator g) :exclude exclude :node-id node-id)))
