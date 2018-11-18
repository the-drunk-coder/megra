(in-package :megra)

;; some helpers
(defun remove-nth (n list)
  (nconc (subseq list 0 n) (nthcdr (1+ n) list)))

(defun purge-keyword (keyword seq)
  (remove keyword (remove-nth (+ 1 (position keyword seq)) seq)))

(defun purge-all-keywords (keywords seq)
  (let ((accum seq))
    (mapcar #'(lambda (kw) (setf accum (purge-keyword kw accum))) keywords)
    accum))

(defmacro define-category-sampling-event (name dur)
  `(progn
     (defun ,(read-from-string name) (&rest rest)
       (let* ((args (copy-seq rest))
	      (lvl (find-keyword-val :lvl args :default 0.4))
	      (dur (find-keyword-val :dur args :default ,dur))	      
	      (pos (find-keyword-val :pos args :default 0.5))	      
	      (start (find-keyword-val :start args :default 0.0))
	      (rate (find-keyword-val :rate args :default 1.0))
	      (atk (find-keyword-val :atk args :default 1))
	      (rel (find-keyword-val :rel args :default 7))
	      (hp-freq (find-keyword-val :hp-freq args :default 10))
	      (hp-q (find-keyword-val :hp-q args :default 0.4))
	      (lp-q (find-keyword-val :lp-q args :default 0.1))
	      (lp-freq (find-keyword-val :lp-freq args :default 5000))
	      (lp-dist (find-keyword-val :lp-dist args :default 1.0))
	      (lp-freq-lfo-depth (find-keyword-val :lp-freq-lfo-depth args :default 0.0))
	      (lp-freq-lfo-speed (find-keyword-val :lp-freq-lfo-speed args :default 0.0))
	      (lp-freq-lfo-phase (find-keyword-val :lp-freq-lfo-phase args :default 0.0))
	      (pf-q (find-keyword-val :pf-q args :default 10))
	      (pf-freq (find-keyword-val :pf-freq args :default 1000))
	      (pf-gain (find-keyword-val :pf-gain args :default 0.0))
	      (rev (find-keyword-val :rev args :default 0.0))
	      (tags (find-keyword-val :tags args :default '(,(read-from-string name))))
	      (combi-fun (find-keyword-val :combi-fun args :default #'replace-value))
	      (param-keywords (loop for key in rest if (typep key 'keyword) collect key))
	      (search-keywords (purge-all-keywords param-keywords rest)))
	 (grain (string-downcase ,name)
		(get-matching-sample-name ,name search-keywords)
		:dur dur
		:lvl lvl :rate rate :start start :atk atk :rel rel
		:lp-dist lp-dist :lp-freq lp-freq :rev rev :pos pos
		:tags tags 
		:hp-freq hp-freq :hp-q hp-q :lp-q lp-q :pf-q pf-q
	        :lp-freq-lfo-depth lp-freq-lfo-depth
		:lp-freq-lfo-speed lp-freq-lfo-speed
		:lp-freq-lfo-phase lp-freq-lfo-phase
		:pf-freq pf-freq :pf-gain pf-gain
		:combi-fun combi-fun)))       
     (defun ,(read-from-string (concatenate 'string name "-p")) (event)
       (member ',(read-from-string name) (event-tags event)))))

(defmacro define-category-sampling-event-4ch (name dur)
  (let ((name-proc (concatenate 'string name "-4ch")))
    `(progn
     (defun ,(read-from-string name-proc) (&rest rest)
       (let* ((args (copy-seq rest))
	      (lvl (find-keyword-val :lvl args :default 0.4))
	      (dur (find-keyword-val :dur args :default ,dur))
	      (pos (find-keyword-val :pos args :default 0.5))
	      (start (find-keyword-val :start args :default 0.0))
	      (rate (find-keyword-val :rate args :default 1.0))
	      (atk (find-keyword-val :atk args :default 1))
	      (rel (find-keyword-val :rel args :default 7))
	      (hp-freq (find-keyword-val :hp-freq args :default 10))
	      (hp-q (find-keyword-val :hp-q args :default 0.4))
	      (lp-q (find-keyword-val :lp-q args :default 0.1))
	      (lp-freq (find-keyword-val :lp-freq args :default 5000))
	      (lp-dist (find-keyword-val :lp-dist args :default 1.0))
	      (lp-freq-lfo-depth (find-keyword-val :lp-freq-lfo-depth args :default 0.0))
	      (lp-freq-lfo-speed (find-keyword-val :lp-freq-lfo-speed args :default 0.0))
	      (lp-freq-lfo-phase (find-keyword-val :lp-freq-lfo-phase args :default 0.0))
	      (pf-q (find-keyword-val :pf-q args :default 10))
	      (pf-freq (find-keyword-val :pf-freq args :default 1000))
	      (pf-gain (find-keyword-val :pf-gain args :default 0.0))
	      (rev (find-keyword-val :rev args :default 0.0))
	      (tags (find-keyword-val :tags args :default '(,(read-from-string name-proc))))
	      (combi-fun (find-keyword-val :combi-fun args :default #'replace-value))
	      (param-keywords (loop for key in rest if (typep key 'keyword) collect key))
	      (search-keywords (purge-all-keywords param-keywords rest)))
	 (grain-4ch (string-downcase ,name)
		    (get-matching-sample-name ,name search-keywords)
		    :dur dur
		    :lvl lvl :rate rate :start start :atk atk :rel rel
		    :lp-dist lp-dist :lp-freq lp-freq :rev rev :pos pos
		    :tags tags
		    :hp-freq hp-freq
		    :hp-q hp-q :lp-q lp-q :pf-q pf-q
		    :lp-freq-lfo-depth lp-freq-lfo-depth
		    :lp-freq-lfo-speed lp-freq-lfo-speed
		    :lp-freq-lfo-phase lp-freq-lfo-phase
		    :pf-freq pf-freq :pf-gain pf-gain
		    :combi-fun combi-fun)))       
     (defun ,(read-from-string (concatenate 'string name-proc "-p")) (event)
       (member ',(read-from-string name) (event-tags event))))))

(defmacro define-category-sampling-event-8ch (name dur)
  (let ((name-proc (concatenate 'string name "-8ch")))
    `(progn
     (defun ,(read-from-string name-proc) (&rest rest)
       (let* ((args (copy-seq rest))
	      (lvl (find-keyword-val :lvl args :default 0.4))
	      (dur (find-keyword-val :dur args :default ,dur))
	      (pos (find-keyword-val :pos args :default 0.5))
	      (start (find-keyword-val :start args :default 0.0))
	      (rate (find-keyword-val :rate args :default 1.0))
	      (atk (find-keyword-val :atk args :default 1))
	      (rel (find-keyword-val :rel args :default 7))
	      (hp-freq (find-keyword-val :hp-freq args :default 10))
	      (hp-q (find-keyword-val :hp-q args :default 0.4))
	      (lp-q (find-keyword-val :lp-q args :default 0.1))
	      (lp-freq (find-keyword-val :lp-freq args :default 5000))
	      (lp-dist (find-keyword-val :lp-dist args :default 1.0))
	      (lp-freq-lfo-depth (find-keyword-val :lp-freq-lfo-depth args :default 0.0))
	      (lp-freq-lfo-speed (find-keyword-val :lp-freq-lfo-speed args :default 0.0))
	      (lp-freq-lfo-phase (find-keyword-val :lp-freq-lfo-phase args :default 0.0))
	      (pf-q (find-keyword-val :pf-q args :default 10))
	      (pf-freq (find-keyword-val :pf-freq args :default 1000))
	      (pf-gain (find-keyword-val :pf-gain args :default 0.0))
	      (rev (find-keyword-val :rev args :default 0.0))
	      (tags (find-keyword-val :tags args :default '(,(read-from-string name-proc))))
	      (combi-fun (find-keyword-val :combi-fun args :default #'replace-value))
	      (param-keywords (loop for key in rest if (typep key 'keyword) collect key))
	      (search-keywords (purge-all-keywords param-keywords rest)))
	 (grain-8ch (string-downcase ,name)
		    (get-matching-sample-name ,name search-keywords)
		    :dur dur
		    :lvl lvl :rate rate :start start :atk atk :rel rel
		    :lp-dist lp-dist :lp-freq lp-freq :rev rev :pos pos
		    :tags tags 
		    :hp-freq hp-freq :hp-q hp-q :lp-q lp-q :pf-q pf-q
		    :lp-freq-lfo-depth lp-freq-lfo-depth
		    :lp-freq-lfo-speed lp-freq-lfo-speed
		    :lp-freq-lfo-phase lp-freq-lfo-phase
		    :pf-freq pf-freq :pf-gain pf-gain
		    :combi-fun combi-fun)))       
     (defun ,(read-from-string (concatenate 'string name-proc "-p")) (event)
       (member ',(read-from-string name) (event-tags event))))))

(defmacro define-category-sampling-event-ambi (name dur)
  (let ((name-proc (concatenate 'string name "-ambi")))
    `(progn
     (defun ,(read-from-string name-proc) (&rest rest)
       (let* ((args (copy-seq rest))
	      (lvl (find-keyword-val :lvl args :default 0.4))
	      (dur (find-keyword-val :dur args :default ,dur))
	      (azi (find-keyword-val :pos args :default 0.0))
	      (ele (find-keyword-val :pos args :default 1.57))
	      (start (find-keyword-val :start args :default 0.0))
	      (rate (find-keyword-val :rate args :default 1.0))
	      (atk (find-keyword-val :atk args :default 1))
	      (rel (find-keyword-val :rel args :default 7))
	      (hp-freq (find-keyword-val :hp-freq args :default 10))
	      (hp-q (find-keyword-val :hp-q args :default 0.4))
	      (lp-q (find-keyword-val :lp-q args :default 0.1))
	      (lp-freq (find-keyword-val :lp-freq args :default 5000))
	      (lp-dist (find-keyword-val :lp-dist args :default 1.0))
	      (lp-freq-lfo-depth (find-keyword-val :lp-freq-lfo-depth args :default 0.0))
	      (lp-freq-lfo-speed (find-keyword-val :lp-freq-lfo-speed args :default 0.0))
	      (lp-freq-lfo-phase (find-keyword-val :lp-freq-lfo-phase args :default 0.0))
	      (pf-q (find-keyword-val :pf-q args :default 10))
	      (pf-freq (find-keyword-val :pf-freq args :default 1000))
	      (pf-gain (find-keyword-val :pf-gain args :default 0.0))
	      (rev (find-keyword-val :rev args :default 0.0))
	      (tags (find-keyword-val :tags args :default '(,(read-from-string name-proc))))
	      (combi-fun (find-keyword-val :combi-fun args :default #'replace-value))
	      (param-keywords (loop for key in rest if (typep key 'keyword) collect key))
	      (search-keywords (purge-all-keywords param-keywords rest)))
	 (grain-ambi (string-downcase ,name)
		     (get-matching-sample-name ,name search-keywords)
		     :dur dur
		     :lvl lvl :rate rate :start start :atk atk :rel rel
		     :lp-dist lp-dist :lp-freq lp-freq :rev rev
		     :tags tags
		     :azi azi :ele ele
		     :hp-freq hp-freq :hp-q hp-q :lp-q lp-q :pf-q pf-q
		     :lp-freq-lfo-depth lp-freq-lfo-depth
		     :lp-freq-lfo-speed lp-freq-lfo-speed
		     :lp-freq-lfo-phase lp-freq-lfo-phase
		     :pf-freq pf-freq :pf-gain pf-gain
		     :combi-fun combi-fun)))       
     (defun ,(read-from-string (concatenate 'string name-proc "-p")) (event)
       (member ',(read-from-string name) (event-tags event))))))


(defun average-sample-length-ms (categ)
  (let ((accum 0.0)
	(count 0)
	file)
    (loop for path in
	 (cl-fad::list-directory (concatenate 'string cm::*sample-root*
					      (string-downcase categ)))
       do (cffi::with-foreign-object (sfinfo '(:struct cl-libsndfile::SF_INFO))
	    (setf file
		  (cl-libsndfile::sf_open (namestring path)
					  cl-libsndfile::SFM_READ sfinfo))
	    (let* ((frames (cffi::foreign-slot-value sfinfo
			    '(:struct cl-libsndfile::SF_INFO)
			    'cl-libsndfile::frames))
		   (sr (cffi::foreign-slot-value sfinfo
						 '(:struct cl-libsndfile::SF_INFO)
						 'cl-libsndfile::samplerate))
		   (length-in-ms (floor (* 1000.0 (/ frames sr)))))
	      ;;(print path)
	      (incf count)
	      (setf accum (+ accum length-in-ms)))
	    (cl-libsndfile::sf_close file)))
    (floor (/ accum count))))

(defun is-file (path)
  (pathname-name (probe-file path)))

;; create an event constructor for each sample category ... 
(loop for path in
     (cl-fad::list-directory cm::*sample-root*)
   when (not (is-file path))
   do (let ((dirname (car (last (pathname-directory path)))))
	(when (and (not (equal dirname "IR"))
		   (not (equal dirname ".git")))
	  (let* ((avg (average-sample-length-ms dirname))
		 (dur (cond ((< avg 64) 64)
			   ((> avg 1024) 1024)
			   (t (float (floor (/ avg 4)))))))
	    (eval `(define-category-sampling-event ,dirname ,dur))
	    (eval `(define-category-sampling-event-ambi ,dirname ,dur))
	    (eval `(define-category-sampling-event-4ch ,dirname ,dur))
	    (eval `(define-category-sampling-event-8ch ,dirname ,dur))))))
