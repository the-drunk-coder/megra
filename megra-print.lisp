;; methods to turn internal object representation back to megra code

;; (in-package :megra)
;; i could split transition print here, but i think it's ok like that for now ...
(defmethod print-edge ((e edge) &key)
  (format nil "(edge ~d ~d :prob ~d :dur ~d)"
	  (edge-source e)
	  (edge-destination e)
	  (edge-probablity e)
	  (transition-duration (car (edge-content e)))))

;;(print-edge (edge 2 1 :dur 512 :prob 100))

(defun print-tags (tags)
  (if tags
      (format nil " :tags '(~{~a ~})" tags)
      ""))

(defun print-combi-fun (fun)
  ;; sbcl-specific ??
  (format nil " :combi-fun #'~a" (print-function-name fun)))

(defun print-function-name (fun)
  (format nil "~a"
	  (nth 2 (multiple-value-list
		  (function-lambda-expression fun)))))

(defmethod print-event ((d duration-event) &key)
  (format nil "(dur ~d~a~a)"
	  (event-duration d)
	  (print-tags (event-tags d))
	  (print-combi-fun (value-combine-function d))))

;;(print-event (dur 512))

(defmethod print-event ((l level-event) &key)
  (format nil "(lvl ~d~a~a)"
	  (event-level l)
	  (print-tags (event-tags l))
	  (print-combi-fun (value-combine-function l))))

;;(print-event (lvl 0.5 :tags '(blue)))

(defmethod print-event ((p pitch-event) &key)
  (format nil "(pitch ~a~a~a~a)"
	  (if (typep (event-pitch p) 'integer)
	      ""
	      "'")	  
	  (event-pitch p)
	  (print-tags (event-tags p))
	  (print-combi-fun (value-combine-function p))))

;;(print-event (pitch 'a3 :tags '(blue)))
;;(print-event (pitch '44 :tags '(blue)))

(defmethod print-event ((s spatial-event) &key)
  (if (event-ambi-p s)
      (format nil "(ambi-pos ~a ~a :dist ~a~a~a)"	  	  
	      (event-azimuth s)
	      (event-elevation s)
	      (event-distance s)
	      (print-tags (event-tags s))
	      (print-combi-fun (value-combine-function s)))
      (format nil "(pos ~a ~a~a)"	  	  
	      (event-position s)
	      (print-tags (event-tags s))
	      (print-combi-fun (value-combine-function s)))))

;;(print-event (pos 0.5 :tags '(blue)))
;;(print-event (ambi-pos 0.5 0.5 :tags '(blue)))

(defmethod print-event ((r rate-event) &key)
  (format nil "(rate ~d~a~a)"
	  (event-rate r)
	  (print-tags (event-tags r))
	  (print-combi-fun (value-combine-function r))))

;;(print-event (rate 0.3 :tags '(blue)))

(defmethod print-event ((s start-event) &key)
  (format nil "(start ~d~a~a)"
	  (event-start s)
	  (print-tags (event-tags s))
	  (print-combi-fun (value-combine-function s))))

;;(print-event (start 0.3 :tags '(blue)))

(defmethod print-event ((s message-event) &key)
  (format nil "(string-event ~d~a)"
	  (event-message s)
	  (print-tags (event-tags s))))

;;(print-event (string-event "hi"))

(defmethod print-event ((g grain-event) &key)
  (format nil "(grain-event \"~a\" \"~a\" :dur ~a :lvl ~a :start ~a :rate ~a  
                        :hp-freq ~a :hp-q ~a
                        :pf-freq ~a :pf-q ~a :pf-gain ~a
                        :lp-freq ~a :lp-q ~a :lp-dist ~a
                        :atk ~a :rel ~a
                        :rev ~a
                        :azi ~a :ele ~a :pos ~a
                        :ambi ~a~a~a)"
	  (event-sample-folder g)
	  (event-sample-file g)
	  (event-duration g)
	  (event-level g)
	  (event-start g)
	  (event-rate g)
	  (event-hp-freq g)
	  (event-hp-q g)
	  (event-pf-freq g)
	  (event-pf-q g)
	  (event-pf-gain g)
	  (event-lp-freq g)
	  (event-lp-q g)
	  (event-lp-dist g)
	  (event-attack g)
	  (event-release g)
	  (event-reverb g)
	  (event-azimuth g)
	  (event-elevation g)
	  (event-position g)
	  (event-ambi-p g)
	  (print-tags (event-tags g))
	  (print-combi-fun (value-combine-function g))))

;;(print-event (grain "bra" "bla"))

(defmethod print-event ((m midi-event) &key)
  (format nil "(mid ~a~D :lvl ~a :dur ~a~a~a)"
	  (if (typep (event-pitch m) 'integer)
	      ""
	      "'")
	  (event-pitch m)
	  (event-level m)
	  (event-duration m)
	  (print-tags (event-tags m))
	  (print-combi-fun (value-combine-function m))))

;;(print-event (mid 'a2 :tags '(blue)))

(defmethod print-node ((n node) &key)
  (if (eql (node-color n) 'white)
      (format nil "(node ~a ~a)"
	      (node-id n)
	      (format nil "~{~a ~}" (mapcar #'print-event (node-content n))))
      (format nil "(node-col ~a (:col '~a) ~a)"
	      (node-id n)
	      (node-color n)
	      (format nil "~{~a ~}" (mapcar #'print-event (node-content n))))))

;;(print-node (node 1 (mid 'a2) (pitch 'a2)))
;;(print-node (node-col 1 (:col 'green) (mid 'a2) (pitch 'a2)))

(defmethod print-graph ((g graph-event-processor) &key)
  (format nil "(graph '~a (:perma ~a :combine-mode '~a :combine-filter #'~a)~%~{~a~}~{~a~})"
	  (graph-id (source-graph g))
	  (copy-events g)
	  (combine-mode g)
	  (print-function-name (combine-filter g))	 
	  ;; might save hashtable access here ... 
	  (loop for key being the hash-keys of (graph-nodes (source-graph g))
	     collect (format nil "~C~a~%"
			     #\tab
			     (print-node (gethash key (graph-nodes (source-graph g))))))	  
	  (loop for key being the hash-keys of
	       (graph-edges (source-graph g))
	     append
	       (mapcar #'(lambda (edge) (format nil "~C~a~%"
			    #\tab
			    (print-edge edge)))
		       (gethash key (graph-edges (source-graph g)))))))
	  
;;(graph 'test ()
;;  (node 1 (mid 'a2 :tags '(blue)))
;;  (node 2 (mid 'a3))
;;  (edge 1 2 :dur 512 :prob 50)
;;  (edge 1 1 :dur 512 :prob 50)
;;  (edge 2 1 :dur 512 :prob 100))

;; (format t (print-graph (gethash 'test *processor-directory*)))






     

