(in-package :megra)


;; Control your samples more !
;; Don't start with boom/tschak !
;; Occasionally dial back on speed!
;; Don't use the saw synth!
;; Start only with samples!
;; Use more filters and positioning !
;; Use 'learn' more often !
;; Lifemodel on parameter rows !
;; LISTEN !
;; use nuc/inh/grow !
;; Some more funky beats !
;; Try to approach music as a whole, avoid track thinking !
;; Single-chains that are a piece on their own ! 
;; Event expansion macros ?
;; go orchestral !

(ql:quickload :hunchentoot)
(in-package :megra)

;; define acceptor
(defparameter *acc* nil)

(defun start-vis-server ()
  (setf *acc* (make-instance 'hunchentoot:easy-acceptor :port 4242 :document-root (pathname "vis") :access-log-destination nil :message-log-destination nil))
  (hunchentoot:start *acc*))

(defun stop-vis-server ()
  (hunchentoot:stop *acc*))

;; get processor list
(hunchentoot:define-easy-handler (print-list :uri "/list") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~{~a;~}" (alexandria::hash-table-keys *processor-directory*)))

;; get dot code for a processor ...
(hunchentoot:define-easy-handler (get-proc :uri "/proc") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((key (find-symbol (string-upcase name) :megra)))
    (if (gethash key *processor-directory*)
        (let ((s (make-string-output-stream)))
          (to-plain-dot (gethash key *processor-directory*) :output s)
          (format nil "~D" (get-output-stream-string s)))        
        (format nil "~D nonexistent" name))))

;; get dot code for a processor ...
(hunchentoot:define-easy-handler (get-all :uri "/all") ()
  (setf (hunchentoot:content-type*) "text/plain")  
  (let ((s (make-string-output-stream)))
    (loop for p being the hash-values of *processor-directory*
          do (to-plain-dot p :output s)
          do (format s "~%"))    
    (format nil "~D" (get-output-stream-string s))))








