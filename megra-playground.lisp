(require 'cm)
(in-package :cm)

;; initialize -- seems like it has to be like this ...
(progn
    (incudine:rt-start)
    (sleep 1)
    (midi-open-default :direction :input)
    (midi-open-default :direction :output)
    (osc-open-default :host "127.0.0.1" :port 3002 :direction :input)
    (osc-open-default :host "127.0.0.1" :port 3003 :direction :output)    
    (setf *out* (cm::new cm::incudine-stream))
    (setf *rts-out* *out*))

;; then load the megra dsp stuff .. wait until compilation has finished !!
(compile-file "megra-dsp")
(load "megra-dsp")

;; now everything should be ready to load the megra package ... 
(load "megra-package")

(in-package :megra)

;; define some test graph structures
(graph 'uno-midi ()
       (node 1 (mid 65 :lvl .8 :dur 200))
       (node 2 (mid 81 :lvl 1 :dur 200) (mid 50 :lvl 1 :dur 50))
       (edge 1 2 :prob 100 :dur 400)
       (edge 2 1 :prob 100 :dur 400))

;; dispatch the graph to make it sound 
;; the empty parentheses are the space for additional options
;; (which we don't use so far ... )
(dispatch () 'uno-midi) 

;; deactivate to make it stop
(deactivate 'uno-midi)

;; use clear to stop and clear all currently playing objects 
(clear)

;; individual graphs are basically first-order markov chains ...
(graph 'dos-midi ()
       (node 1 (mid 59 :lvl .8 :dur 1500))
       (node 2 (mid 73 :lvl .9 :dur 1500))
       (node 3 (mid 78 :lvl .9 :dur 350))
       (edge 1 2 :prob 50 :dur 500)
       (edge 1 3 :prob 50 :dur 750)
       (edge 2 1 :prob 100 :dur 500)
       (edge 3 1 :prob 100 :dur 250))

(dispatch () 'dos-midi)

(deactivate 'dos-midi)

;; you can also dispatch multiple graphs at the same time, the event
;; streams will be merged (for now) ...
;; in the future, the event streams might also be combined according to
;; certain rules (which i have to figure out yet)
;; in this case, the last graph in the case determines the timing
(dispatch ()
  'uno-midi
  'dos-midi)

;; deactivate the object in the chain closest to the dispatcher to make it stop ...  
(deactivate 'uno-midi)

;; use the grain event to play a (or parts of) a soundfile
(graph 'the-grain (:perma t) 
       (node 1 (grain "misc" "tada" :dur 256 :lvl 0.5 :rate 0.5 :atk 64 :rel 64))
       (edge 1 1 :prob 100 :dur 64))

(graph 'the-512-beat ()
       (node 1 (grain "03_electronics" "01_808_long_kick" :dur 256
		      :lvl 1.0 :rate 1.1 :start 0.01 :atk 0.001 :lp-dist 1.0 :lp-freq 5000))
       (node 2 (grain "03_electronics" "08_fat_snare" :dur 128 :atk 0.1 :lvl 0.5 :rate 0.4))
       (edge 1 2 :prob 100 :dur 256)
       (edge 2 1 :prob 100 :dur 256))

(dispatch () 'the-grain)
(dispatch () 'the-512-beat)

(deactivate 'the-grain)
(deactivate 'the-512-beat)

;; MODIFIERS -- hook modifiers into the chain to manipulate the sound.
(graph 'tres-midi ()
       (node 1 (mid 84 :lvl .9 :dur 50))
       (edge 1 1 :prob 100 :dur 100))

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

;; hook an event modifier into the chain ...
(dispatch
 'tres-midi
 (brownian-motion 'tres-rw 'pitch :step 5 :ubound 84 :lbound 50 :wrap t)
 'uno-midi)


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
  (brownian-motion 'tres-rw 'pitch :step 3 :ubound 84 :lbound 50 :wrap t)
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
  (brownian-motion 'tres-rw 'pitch :step 5 :ubound 84 :lbound 50 :wrap t)
  'tres-midi)

(dispatch (:unique nil)
  (spigot 'tap-g :flow t) ;; spigot helps in the development process ... 
  ;;(oscillate-between 'tres-osc 'lvl 0.0 1.0 :cycle 30)
  'tres-midi)


;; TBD:
;; track phase offset per event source for oscillate-between
;; arranging modifiers in graphs ...
;; define meaningful behaviour for non-mandatory modifiers ...
;; (chance ...) shortcut ... even though the semantics of "chance" is
;;    slightly different, as is evaluates the chance of something to happen for the current
;;    event, whereas a graph modifies the modification for the next event ... 
;; graph-theory stuff -- graph transformations etc
;; syncstart
;; midi note blocker for disklavier
;; vugs
;; get rid of deactivating error msg ...

;; DONE:
;; define consistent unique/non-unique dispatching ... more or less, it's difficult
;; eventually make multiple dispatching possible ... like, (dispatch :check-active nil ...)
;; fix midi note duration, bzw. make it effective
;; chain rebuilding - if you hook a new effect to the END of the dispatcher chain,
;;     multiple dispatiching will happen !
;; fix de-/reactivating graphs -- stupid mistake ...
;; fix midi handling -- works, seems to be connected to the way of initializing incudine ...
;; (brownian-motion 'tres-rw 'pitch :step 4 :ubound 84 :lbound 50 :wrap t TRACK-STATE: nil) -- makes sense in comination with below ... ??
;; (graph 'xyz :copy-events nil) --- original events are sent out, makes sense in comination with the above
;; oscillating event modifier -- works !
;; tree-like dispatch branching -- works !
;; avoid duplicate dispatches -- works !
;; automatic re-activation -- works !





;; keep for the sake of funcall
(dispatch ()
 (oscillate-between 'lp-freq-b 'lp-freq 100 8000 :cycle 100)
 (oscillate-between 'dist-b 'rate 0.1 1.0 :cycle 400)
 (oscillate-between 'dist-b 'rate 0.1 1.0 :cycle 400)
 (oscillate-between 'q-b 'lp-q 0.1 1.0 :cycle 50) 
 'the-512-beat)

(dispatch ()
 (brownian-motion 'start-b 'start :step 0.001 :ubound 0.001 :lbound 0.8 :wrap t)
 (oscillate-between 'lp-freq-c 'lp-freq 100 8000 :cycle 1000)
 (oscillate-between 'q-c 'lp-q 0.1 1.0 :cycle 50)
 (oscillate-between 'pos-c 'pos 0.4 0.8 :cycle 50) 
 (oscillate-between 'rate-b 'rate 0.1 0.14 :cycle 400) 
 'the-grain)
