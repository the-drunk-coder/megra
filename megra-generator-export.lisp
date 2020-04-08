(in-package :megra)

(defmethod generator-to-code ((g generator) &key (out-stream nil))
  (format out-stream "(infer '~D ~% :type '~D ~% :events ~% ~{~a~} :rules ~%~{~a~})"
	  (generator-name g)
          (if (typep (inner-generator g) 'vom::naive-pfa) 'naive 'pfa)
	  ;; might save hashtable access here ... 
	  (loop for key being the hash-keys of (event-dictionary g)
	        collect (format nil "'~D ~{~a~}~%" key (mapcar 'print-event (gethash key (event-dictionary g)))))
	  (loop for key being the hash-keys of (vom::children (inner-generator g))
	        nconc (loop for dest in (gethash key (vom::children (inner-generator g)))
                            collect (if (typep (inner-generator g) 'vom::naive-pfa)
                                        (format nil "'(~D ~D ~D)~%" key (cdr dest) (car dest))
                                        (format nil "'(~D ~D ~D)~%" key (alexandria::lastcar (cadr dest)) (round (* 100 (car dest)))))))))


(defun to-code (gen stream &key loadable)
  (let ((act-gen (if (typep gen 'generator) gen (gethash gen *processor-directory*)))
        (filename (cond ((stringp stream) stream) ((symbolp stream) (symbol-name stream)) (t nil)) ) )
    (if filename
        (with-open-file (str (concatenate 'string filename ".megra")
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format str "(in-package :megra)")
          (when loadable (format str "(funcall~%"))
          (generator-to-code act-gen :out-stream str)
          (when loadable (format str ")")))
        (generator-to-code act-gen :out-stream t))))


