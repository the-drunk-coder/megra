(in-package :megra)
;; define some test graph structures
(graph 'uno-midi ()
  (node 1 (mid #'(lambda () (+ 20 (random 60)) ) :lvl .8 :dur 200))
  (node 2 (mid 81 :lvl 1 :dur 200) (mid 50 :lvl 1 :dur 50))
  (edge 1 2 :prob 100 :dur #'(lambda () (+ 100 (random 200))))
  (edge 2 1 :prob 100 :dur 400))

;; dispatch the graph to make it sound 
;; the empty parentheses are the space for additional options
;; (which we don't use so far ... )
(dispatch () 'uno-midi) 

(clear)
;; deactivate to make it stop
(deactivate 'uno-midi)

;; use clear to stop and clear all currently playing objects 
(clear)

(typep (gethash 'dos-midi *processor-directory*) 'graph-event-processor)

;; individual graphs are basically first-order markov chains ...
(graph 'dos-midi ()
       (node 1 (mid 59 :lvl .8 :dur 1500))
       (node 2 (mid 73 :lvl .9 :dur 1500))
       (node 3 (mid 78 :lvl .9 :dur 350))
       (edge 1 2 :prob 50 :dur 1500)
       (edge 1 3 :prob 50 :dur 1750)
       (edge 2 1 :prob 100 :dur 1500)
       (edge 3 1 :prob 100 :dur 1250))

(graph 'uno-midi-synced ()
       (node 1 (mid 65 :lvl .8 :dur 200))
       (node 2 (mid 81 :lvl 1 :dur 200) (mid 50 :lvl 1 :dur 50))
       (edge 1 2 :prob 100 :dur 400)
       (edge 2 1 :prob 100 :dur 400))

(clear)
(dispatch () 'dos-midi)

;; sync dispatching, will be performed when the graph specified
;; as sync-to is dispatched
(dispatch (:sync-to 'dos-midi) 'uno-midi-synced)

(deactivate 'dos-midi)
(deactivate 'uno-midi-synced)

;; you can also dispatch multiple graphs at the same time, the event
;; streams will be merged (for now) ...
;; in the future, the event streams might also be combined according to
;; certain rules (which i have to figure out yet)
;; in any case, the last graph in the chain determines the timing
(dispatch ()
  'uno-midi
  'dos-midi)

;; deactivate the object in the chain closest to the dispatcher to make it stop ...  
(deactivate 'uno-midi)
(clear)
;; use the grain event to play a (or parts of) a soundfile
(graph 'the-grain () 
  (node 1 (grain "misc" "tada" :dur 512 :pos 0.0 :lvl 0.5 :rate 0.4 :atk 64 :rel 65 :rev 0.2
		 :ambi nil))
       (edge 1 1 :prob 100 :dur 512))

(dispatch (:sync-to 'the-512-beat)
  ;;(chance-combine 'grain-lvl-cc 0 (lvl 0.0))
  'the-grain)


(deactivate 'the-grain)

(graph 'ambi-test ()
  (node 1 (grain "02_instruments" "pizz_f4" :dur 128 :atk 1 :rel 30
		 :lvl 0.9 :rate 1.0 :rev 0.0
		 :azi #'(oscillate-between 0.0 3.14 :cycle 50)
		 :ele #'(oscillate-between 0.0 3.14 :cycle 60)
		 :ambi t))
  (edge 1 1 :prob 100
	:dur #'()
	))

(dispatch ()
  (spigot 'ambi-tap-512 :flow t)
  (oscillate-between 'osc-azi 'azi 0.0 3.14 :cycle 50)
  (oscillate-between 'osc-ele 'ele 0.0 3.14 :cycle 20)
  'ambi-test)

(deactivate 'ambi-tap-512)

(graph 'the-512-beat ()
       (node 1 (grain "03_electronics" "01_808_long_kick" :dur 512
		      :lvl 1.0 :rate 1.1 :start 0.01 :atk 1 :rel 7
		      :lp-dist 1.0 :lp-freq 5000 :rev 0.0 :ambi nil :pos 0.5))
       (node 2 (grain "03_electronics" "08_fat_snare" :dur 512 :atk 0.1 :pos 0.5
		      :lvl 0.9 :rate 2.4 :rev 0.0 :tags '(snare) :ambi nil))
       (node 3 (grain "03_electronics" "01_808_long_kick" :dur 512
		      :lvl 1.0 :rate 1.1 :start 0.01 :atk 1 :rel 7
		      :lp-dist 1.0 :lp-freq 5000 :rev 0.0 :ambi nil :pos 0.5))     
       (edge 1 2 :prob 100 :dur 512)
       (edge 2 1 :prob 60 :dur 512)
       (edge 2 3 :prob 40 :dur 256)
       (edge 3 3 :prob 40 :dur 256)
       (edge 3 2 :prob 60 :dur 512))

(clear)

(free-all-samples)

(defun is-snare-p (event)
  (member 'snare (event-tags event)))

(dispatch () 'the-grain)

(dispatch ()
  (spigot 'tap-512 :flow t)
  (chance-combine 'grain-lvl-cc 0 (lvl 0.0):filter #'is-snare-p)
  (stream-oscillate-between 'tres-osc 'rate 1.0 2.5 :cycle 10 :filter #'is-snare-p)
  'the-512-beat)

(deactivate 'the-grain)

(deactivate 'tap-512)

;; MODIFIERS -- hook modifiers into the chain to manipulate the sound.
(clear)

(graph 'tres-midi ()
       (node 1 (mid 84 :lvl .9 :dur 50))
       (edge 1 1 :prob 100 :dur 100))

(dispatch ()
  (spigot 'tap-b :flow t) ;; spigot helps in the development process ... 
  (brownian-motion 'tres-rw 'pitch :step-size 3 :ubound 84 :lbound 50 :wrap t)  
  (oscillate-between 'tres-osc 'lvl 0.1 0.9 :cycle 100)
  'tres-midi)

;; i'd recommend using a spigot when experiment with the chains, to provide
;; a persistent outlet ... otherwise you might get strange effects ...
;; try uncommenting the elements ...
;; if you set the :flow parameter to nil, dispatching will also stop, but the
;; dispatcher will keep trying to pull (in contrast to deactivating)
(dispatch ()
  (spigot 'tap-b :flow t) ;; spigot helps in the development process ... 
  (brownian-motion 'tres-rw 'pitch :step-size 3 :ubound 84 :lbound 50 :wrap t)  
  (oscillate-between 'tres-osc 'lvl 0.0 1.0 :cycle 300)
  'tres-midi)

;; hook an event modifier into the chain ...
(dispatch ()
 'tres-midi
 (brownian-motion 'tres-rw 'pitch :step-size 5 :ubound 84 :lbound 50 :wrap t)
 'uno-midi)

;; CONDUCTOR GRAPHS -- use a graph to control another graph
(clear)

(graph 'tres-midi ()
       (node 1 (mid 84 :lvl .9 :dur 50))
       (edge 1 1 :prob 100 :dur 100))

;; Use the PSET function to set a parameter in some object.
;; the lambda construction seems a little inconvenient, might
;; be replaced by some macro in the future ... 
(graph 'tres-ctrl ()
  (node 1 (ctrl #'(lambda () (pset 'tres-rw 'step-size (random 10)))))
  (edge 1 1 :prob 100 :dur 5000))

(dispatch ()
  (spigot 'tap-b :flow t) ;; spigot helps in the development process ... 
  (brownian-motion 'tres-rw 'pitch :step-size 3 :ubound 84 :lbound 50 :wrap t)  
  'tres-midi)

(dispatch ()
  'tres-ctrl)

(deactivate 'tres-ctrl)
(deactivate 'tap-b)

;; EXPLAIN - state tracking and perma 

;; UNIQUE vs NON-UNIQUE

;; 1.) unique -- DEFAULT
(clear)

(graph 'tres-midi ()
       (node 1 (mid 84 :lvl .9 :dur 50))
       (edge 1 1 :prob 100 :dur 100))

(dispatch ()
  (spigot 'tap-a :flow t) ;; spigot helps in the development process ... 
  'tres-midi)

(dispatch ()
  (spigot 'tap-b :flow t) ;; spigot helps in the development process ... 
  (brownian-motion 'tres-rw 'pitch :step-size 3 :ubound 84 :lbound 50 :wrap t)
  (oscillate-between 'tres-osc 'lvl 0.0 1.0 :cycle 300)
  'tres-midi)

(dispatch ()
  (spigot 'tap-c :flow t) ;; spigot helps in the development process ... 
  'tres-midi)

;; 2.) non-unique
;; the last graph in the chain determines the timing, so each
;; processor chain needs a unique ending point, but it's possible
;; for multiple processors to have the same predecessor, thus branching the
;; dispatcher chains
(clear)

(graph 'tres-midi ()
       (node 1 (mid 84 :lvl .9 :dur 50))
       (edge 1 1 :prob 100 :dur 200))

(dispatch (:unique nil)
  (spigot 'tap-d :flow t) ;; spigot helps in the development process ... 
  (brownian-motion 'tres-rw 'pitch :step-size 1 :lbound 50 :ubound 60 :wrap t)
  'tres-midi)

(dispatch (:unique nil)
    (spigot 'tap-g :flow t) ;; spigot helps in the development process ... 
    ;;(oscillate-between 'tres-osc 'lvl 0.0 1.0 :cycle 30)
    'tres-midi)

;; CHAIN - define a processor chain without dispatching it, i.e if you want to dispatch
;; it by using a (ctrl ...) event ...
(clear)

(graph 'tres-midi ()
       (node 1 (mid 84 :lvl .9 :dur 50))
       (edge 1 1 :prob 100 :dur 200))

(chain ()     
   (spigot 'tap-b :flow t)
   (oscillate-between 'tres-osc 'lvl 0.0 1.0 :cycle 30)
   (brownian-motion 'tres-rw 'pitch :step-size 3 :ubound 84 :lbound 50 :wrap t)  
   'tres-midi)

;; just remember to set the :chain flag, otherwise your chain will get disconnected ...
(dispatch (:chain t)
	  'tap-b)

(deactivate 'tap-b)


;; TAGS AND FILTERS 
(graph 'tres-midi ()
       (node 1 (mid 84 :lvl .9 :dur 50 :tags '(red)))
       (edge 1 1 :prob 100 :dur 200))

(defun has-red-tag-p (event)
  (member 'red (event-tags event)))

(defun has-blue-tag-p (event)
  (member 'blue (event-tags event)))

(dispatch ()
  (spigot 'tap-fil :flow t)
  (brownian-motion 'tres-rw 'pitch :step-size 3 :ubound 84
		   :lbound 50 :wrap t :filter #'has-red-tag-p)  
  'tres-midi)

;; CONDUCTOR GRAPHS -  create graph-based scores by dispatching chains with a conductor graph
(clear)

(chain ()     
   (spigot 'tap-x :flow t)   
   (graph 'x-midi ()
       (node 1 (mid 84 :lvl .9 :dur 50))
       (edge 1 1 :prob 100 :dur 200)))


(chain ()     
   (spigot 'tap-y :flow t)   
   (graph 'y-midi ()
       (node 1 (mid 42 :lvl .9 :dur 50))
       (edge 1 1 :prob 100 :dur 400)))

;; in this case, don't delete the spigot, as the graph doesn't recreate the chain ...
(graph 'xy-ctrl ()
  (node 1 (ctrl #'(lambda () (dispatch (:chain t) 'tap-x))))
  (node 2 (ctrl #'(lambda () (dispatch (:chain t) 'tap-y))))
  (node 3 (ctrl #'(lambda () (deactivate 'tap-x :del nil))))
  (node 4 (ctrl #'(lambda () (deactivate 'tap-y :del nil))))
  (edge 1 2 :prob 100 :dur 4000)
  (edge 2 3 :prob 100 :dur 4000)
  (edge 3 4 :prob 100 :dur 4000)
  (edge 4 1 :prob 100 :dur 4000))

(dispatch () 'xy-ctrl)

(deactivate 'xy-ctrl)

;; incrementally build events ...
;; might be understood as a generalization of the
;; serialist paradigm

;; this is a nice one ...
(dispatch ()
  (spigot 'tap-inc :flow t)
  (oscillate-between 'dur-osc 'dur 150 400 :cycle 200 :affect-transition t)
  (graph 'pitcher (:combine-mode 'zip)
    (node 1 (pitch 32))
    (node 2 (pitch 52))
    (edge 1 1 :prob 60)
    (edge 1 2 :prob 40)
    (edge 2 2 :prob 55)
    (edge 2 1 :prob 45))
  (graph 'leveller (:combine-mode 'zip)
    (node 1 (lvl 0.6))
    (node 2 (lvl 1.0))
    (node 3 (lvl 0.3))
    (edge 1 1 :prob 40)
    (edge 1 2 :prob 30)
    (edge 1 3 :prob 30)
    (edge 2 2 :prob 55)
    (edge 2 1 :prob 45)
    (edge 3 1 :prob 100))
  (graph 'durator (:combine-mode 'zip)
    (node 1 (dur 512))
    (node 2 (dur 768))
    (edge 1 1 :prob 40)
    (edge 1 2 :prob 60)
    (edge 2 2 :prob 45)
    (edge 2 1 :prob 55))
  (graph 'origin () ;; for now, origin event needs to have handler ...
    (node 1 (mid 84 :lvl .9 :dur 50))
    (edge 1 1 :prob 100 :dur 1000)))

(encourage-with-tail 'tap-inc)
(graph->code 'pitcher "/home/nik/REPOSITORIES/FREE_RANGE/MEGRA/pitcher-out.lisp")
(graph->code 'durator "/home/nik/REPOSITORIES/FREE_RANGE/MEGRA/leveller-out.lisp")
(graph->code 'leveller "/home/nik/REPOSITORIES/FREE_RANGE/MEGRA/durator-out.lisp")
(graph->code 'origin "/home/nik/REPOSITORIES/FREE_RANGE/MEGRA/origin-out.lisp")

(deactivate 'tap-inc)

(defun has-two-p (event)
  (member 'two (event-tags event)))

;; this is a nice one ...
(dispatch ()
  (spigot 'tap-inc :flow t)  
  (graph 'pitcher (:combine-mode 'zip :combine-filter #'has-two-p)
    (node 1 (pitch 32))
    (node 2 (pitch 52))
    (edge 1 1 :prob 60)
    (edge 1 2 :prob 40)
    (edge 2 2 :prob 55)
    (edge 2 1 :prob 45))
   (graph 'origin () ;; for now, origin event needs to have handler ...
    (node 1 (mid 84 :lvl .9 :dur 50))
    (node 2 (mid 84 :lvl .9 :dur 50))
    (edge 1 2 :prob 100 :dur 1000)
    (edge 2 1 :prob 100 :dur 1000)))

;; another variant, with different value combi functions ...
(dispatch ()
  (spigot 'tap-inc :flow t)  
  (graph 'pitcher (:combine-mode 'zip)
    (node 1 (pitch 10 :combi-fun #'-))
    (node 2 (pitch 30 :combi-fun #'-))
    (edge 1 1 :prob 60)
    (edge 1 2 :prob 40)
    (edge 2 2 :prob 55)
    (edge 2 1 :prob 45))
   (graph 'origin () ;; for now, origin event needs to have handler ...
    (node 1 (mid 84 :lvl 1.0 :dur 50))
    (edge 1 1 :prob 100 :dur 1000)))

;; controller input, designed for AKAI LPD8 
(register-knob 1 #'(lambda (val) (princ val)))

(register-pad 2 #'(lambda (val) (princ val)))

;; the disencourage algorithm 
(graph 'all-to-all ()
       (node 1 (mid 60 :lvl .8 :dur 250))
       (node 2 (mid 62 :lvl .9 :dur 250))
       (node 3 (mid 64 :lvl .9 :dur 250))
       (node 4 (mid 67 :lvl .9 :dur 150))
       (node 5 (mid 70 :lvl .9 :dur 150))       
       (edge 1 1 :prob 20 :dur 250) (edge 1 2 :prob 20 :dur 125) (edge 1 3 :prob 20 :dur 125)
       (edge 1 4 :prob 20 :dur 250) (edge 1 5 :prob 20 :dur 750)        
       (edge 2 1 :prob 20 :dur 500) (edge 2 2 :prob 20 :dur 250) (edge 2 3 :prob 20 :dur 250)
       (edge 2 4 :prob 20 :dur 250) (edge 2 5 :prob 20 :dur 500) 

       (edge 3 1 :prob 20 :dur 500) (edge 3 2 :prob 20 :dur 500) (edge 3 3 :prob 20 :dur 250)
       (edge 3 4 :prob 20 :dur 500) (edge 3 5 :prob 20 :dur 250) 

       (edge 4 1 :prob 20 :dur 500) (edge 4 2 :prob 20 :dur 500) (edge 4 3 :prob 20 :dur 500)
       (edge 4 4 :prob 20 :dur 500) (edge 4 5 :prob 20 :dur 250) 
       
       (edge 5 1 :prob 20 :dur 500) (edge 5 2 :prob 20 :dur 250) (edge 5 3 :prob 20 :dur 500)
       (edge 5 4 :prob 20 :dur 250) (edge 5 5 :prob 20 :dur 250))

(dispatch ()
  'all-to-all)

(deactivate 'all-to-all :del nil)

;; the path is traced ...
(princ (traced-path (gethash 'all-to-all *processor-directory*)))

(graph->code 'all-to-all "/home/nik/REPOSITORIES/FREE_RANGE/MEGRA/ata-out.lisp")
(graph->svg 'all-to-all "/home/nik/REPOSITORIES/FREE_RANGE/MEGRA/ata-vis.dot")

;; encourage or discourage the traced path ...
(progn (encourage 'all-to-all)
       (graph->code 'all-to-all "/home/nik/REPOSITORIES/FREE_RANGE/MEGRA/ata-out.lisp")
       (graph->svg 'all-to-all "/home/nik/REPOSITORIES/FREE_RANGE/MEGRA/ata-vis.dot"))

(progn 
  (discourage 'all-to-all)
  (graph->code 'all-to-all "/home/nik/REPOSITORIES/FREE_RANGE/MEGRA/ata-out.lisp")
  (graph->svg 'all-to-all "/home/nik/REPOSITORIES/FREE_RANGE/MEGRA/ata-vis.dot"))

(graph 'check-midi-note-off ()
  (node 1 (mid 60 :lvl .8 :dur 1000))
  (edge 1 1 :prob 20 :dur 20))

(dispatch () 'check-midi-note-off)

(clear)
;; TBD:
;; functions as parameters ... 
;; megra-mode -- S-c S-s for starting, S-ret for evaluation ... 
;; akita interface
;; graphviz visualizer -> multiple graphs in one svg
;; the uniqueness rule for graphs is not really helpful, imagine if you want
;;    to use a duratoin or level graph with more than one source ... thus,
;;    there need to be either a change in the representation, or some cloning function,
;;    like, (clone 'pitcher) ...
;; combine transitions
;; rethink dispatcher concept ... maybe replace 'is-active' by dispatcher directory ?
;;     especially interesting if step dispatching becomes more than a debugging feature ... 
;; symbols as values, resolve at render time ??
;; live-visualizer
;; yasnippets 
;; automatically deactivate if final state is reached 
;; check why chance-combine doesn't work in any position
;;    (currently seems to work only in end position) 
;; ambisonics panner - sc
;; note names 
;; pass flags for processors, to make programmatic control easier (brownian-motion ... :act t/nil)
;; arranging modifiers in graphs ...
;; 'funnel event combnation mode
;; 'all event combination mode 
;; setting default handlers for incomplete events ? like, if event has pitch -> midi,
;;     if event has only dur -> default sample ? Precedence ? Fallback ?
;; programmatically hook processor in between two others (hook 'proc :post 'bla)
;; merge streams ? (merge 'tap-a 'tap-b) -- POSSIBLE only if streams have same origin ...
;; parameter faders/modders - anonymous
;;    (mid 84 :lvl '(fade 0.2 0.4 :step 10)) -> step inc after each (handle-event)
;;    (oscillate-between ... :ubound '(fade 0.4 :step 400)
;;    needed to be temporarily replaced by current value:
;;        before event evaluation
;;        before event modification (i.e. before the modifier acts)
;;     CONFLICT: what if event passes through modifier ? 
;; track phase offset per event source for oscillate-between
;; define meaningful behaviour for non-mandatory modifiers ...
;; (chance ...) shortcut ... even though the semantics of "chance" is
;;    slightly different, as is evaluates the chance of something to happen for the current
;;    event, whereas a graph modifies the modification for the next event ... 
;; graph-theory stuff -- graph transformations etc
;; syncstart (for live use mainly ...)
;; midi note blocker for disklavier
;; more vugs

;; DONE:
;; only encourage/discourage processors that are active or in a chain ...
;; midi latency - that was easy ... thanks cm !
;; make files loadable ... (check, seems ok ...)
;; graphviz svg visualizer
;; text output
;; disencourage - longer trace, but drop first ??
;; tracing/ trace/ encourage /discourage
;; filter for graph combi !! -- tag based
;; ambisonics panner - incudine 
;; event tags, like (mid 84 :lvl .9 :dur 30 :tags '(foo bar))
;; filters, like (oscillate-between ... :filter #'has-foo-tag)
;;  -- hook those into the same place where you check if event has slot !
;; for modifying-event-processors: check if modified property is present at all ...
;; make modifiers work on transition duration 
;; event combination - 't was a piece of work ... 
;; get rid of deactivating error msg ...
;; chain procs without dispatching -- done
;; define consistent unique/non-unique dispatching ... more or less, it's difficult
;; eventually make multiple dispatching possible ... like, (dispatch :check-active nil ...)
;; fix midi note duration, bzw. make it effective
;; chain rebuilding - if you hook a new effect to the END of the dispatcher chain,
;;     multiple dispatiching will happen !
;; fix de-/reactivating graphs -- stupid mistake ...
;; fix midi handling -- works, seems to be connected to the way of initializing incudine ...
;; (brownian-motion 'tres-rw 'pitch :step 4 :ubound 84 :lbound 50 :wrap t TRACK-STATE: nil)
;;            -- makes sense in comination with below ... ??
;; (graph 'xyz :copy-events nil) --- original events are sent out,
;;          makes sense in comination with the above
;; oscillating event modifier -- works !
;; tree-like dispatch branching -- works !
;; avoid duplicate dispatches -- works !
;; automatic re-activation -- works !
;; node color, inherited by events as tag ...

;; keep this for the sake of funcall
(dispatch ()
 (oscillate-between 'lp-freq-b 'lp-freq 100 8000 :cycle 100)
 (oscillate-between 'dist-b 'rate 0.1 1.0 :cycle 400)
 (oscillate-between 'dist-b 'rate 0.1 1.0 :cycle 400)
 (oscillate-between 'q-b 'lp-q 0.1 1.0 :cycle 50) 
 'the-512-beat)

(dispatch ()
  (spigot 'tada-tap :flow t)
  (brownian-motion 'start-b 'start :step-size 0.001 :ubound 0.001 :lbound 0.8 :wrap t)
  (oscillate-between 'lp-freq-c 'lp-freq 100 8000 :cycle 1000)
  (oscillate-between 'q-c 'lp-q 0.1 1.0 :cycle 50)
  (oscillate-between 'pos-c 'pos 0.4 0.8 :cycle 50) 
  (oscillate-between 'rate-b 'rate 0.1 0.14 :cycle 400) 
  'the-grain)






