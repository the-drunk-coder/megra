# Mégra Function Reference

Table of Contents
=================

* [always - Event Stream Modificator Probablity](#always---event-stream-modificator-probablity)           
* [brownian - Bounded Brownian Motion](#brownian---bounded-brownian-motion)   
* [chop - Chop a sample](#chop---chop-a-sample)
* [clear - Clear Session](#clear---clear-session)
* [cmp - Compose Generators](#cmp---compose-generators)
* [ctrl - Control Functions](#ctrl---control-functions)
* [cyc - Cycle Generator](#cyc---cycle-generator)
* [cyc2 - Cycle Generator](#cyc2---cycle-generator)
* [discourage - Stir Up Generator](#discourage---stir-up-generator)
* [encourage - Consolidate Generator](#encourage---consolidate-generator)
* [env - Parameter Envelope](#env---parameter-envelope)
* [evr - Count-Based Generator Manipulators](#evr---count-based-generator-manipulators)
* [exh - Event Stream Manipulator](#exh---event-stream-manipulator)
* [fade - Parameter Fader](#fade---parameter-fader)
* [grow - Enlarge Generator](#grow---enlarge-generator)
* [grown - Enlarge Generator n times](#grown---enlarge-generator-n-times)
* [haste - speed up evaluation](#haste---speed-up-evaluation)
* [infer - Infer Generator from Rules](#sinfer---infer-generator-from-rules)
* [inh - Event Stream Manipulator](#inh---event-stream-manipulator)
* [life - Manipulate Generator](#lifemodel---manipulate-generator)
* [nuc - Nucleus Generator](#nuc---nucleus-generator)
* [nuc2 - Nucleus Generator](#nuc2---nucleus-generator)
* [oscil - Parameter Oscillator](#oscil---parameter-oscillator)
* [pear - Apply Modifiers](#pear---apply-modifiers)
* [probctrl - Manipulate Generator](#probctrl---manipulate-generator)
* [prob - Event Stream Manipulator Probablity](#prob---event-stream-manipulator-probablity)
* [pprob - Event Stream Manipulator Probablity](#pprob---event-stream-manipulator-probablity)
* [pseq - Event Sequence Generated from Parameters](#pseq---event-sequence-generated-from-parameters)
* [relax - Slow Down Generator](#relax---slow-down-generator)
* [shrink - Shrink Generator](#shrink---shrink-generator)
* [skip - Skip Events](#skip---skip-events)
* [sx - Event Sinks](#sx---multiple-event-sinks)
* [learn - Learn Generator from Distribution](#slearn---learn-generator-from-distribution)
* [stop - Stop Event Processing](#stop---stop-event-processing)
* [xdup - Multiply Generators with Modifiers](#xdup---multiply-generators-independently)
* [xspread2 - Multiply Generators with Modifiers, Spread over Stereo Spectrum](#xdup---multiply-generators-independently)

## `always` - Event Stream Modificator Probablity

Applies an event stream modificator with probability one.

### Parameters

* modificators (list)

### Syntax

```lisp
(always <modificators>)
```

### Examples

Always apply reverb to events:

```lisp
(sx 'some t
  (always (rev 0.1))
  (cyc 'beat "bd ~ ~ ~ sn ~ ~ ~"))
```
## `brownian` - Bounded Brownian Motion 

Define a bounded brownian motion on a parameter.

### Parameters

* lower boundary (float)
* upper boundary (float)
* `:wrap` (boolean) (t) - wrap value if it reaches lower or upper boundary
* `:limit` (boolean) (nil) - limit value if it reaches upper or lower boundary
* `:step-size` (float) (0.1) - step that the parameter will be incremented/decremented

### Syntax

```lisp
(brownian <lower boundary> <upper boundary> :wrap <wrap> :limit <limit> :step-size <step-size>)
```

### Examples

```lisp
(sx 'some t
  (always (rate (brownian 0.8 1.2)))
  (nuc 'violin (violin 'a3)))
```

## `chop` - Chop a sample

Chop a sample into parts, that will be played as a loop.

### Examples

```lisp
(sx 'some t
  (chop 'chops (violin 'a3) 8)) ;; chop violin sample into 8 parts
```

## `clear` - Clear Session

Stops and deletes all present generators.

## `cmp` - Compose Generators

### Examples

```lisp
;; this is nice and functional, but hard to disable individual
;; parts ...
(pear (rate 0.2)
      (evr 20 (haste 2 0.5)
           (cyc 'bl "bd ~ ~ sn ~ ~"))) 

;; with cmp, it can be re-written as:
(cmp
    (pear (rate 0.2))
    (evr 20 (haste 2 0.5))
    (cyc 'bl "bd ~ ~ sn ~ ~")) 
;; now individual modifiers can easily be commented out

```

## `ctrl` - Control Functions

Executes any function, can be used to conduct execution of generators.

### Parameters

* function

### Syntax

```lisp
(ctrl <function>)
```

### Example

```lisp
(defvar part-a (cmp
                 (always (rev 0.1))
                 (nuc 'violin (violin 'a3))))

(defvar part-b (cmp
                 (always (rev 0.1))
                 (nuc 'cello (cello 'c1))))

(sx 'control t
  (cyc 'conduct
     (list
        (ctrl #'(lambda () (sx 'orch t part-a))) 3000
        (ctrl #'(lambda () (sx 'orch t part-b))) 3000)))

```

## `cyc` - Cycle Generator

Generates a cycle (aka loop) from a simple sequencing language.

### Parameters

* name - generator name
* sequence - sequence description
* `:dur` - default space between events 
* `:rep` - probability of repeating an event
* `:max-rep` - limits number of repetitions
* `:rnd` - random connection probability

### Syntax

```lisp
(cyc <name> <sequence> :dur <duration> :rep <repetition probability> :max-rep <max number of repetitions> :rnd <random connection prob>)
```

### Example 
```lisp
(sx 'simple t
  (cyc 'beat "bd ~ hats ~ sn ~ hats ~"))
```

## `cyc2` - Cycle Generator

Generates a cycle (aka loop) from a simple sequencing language, using the advanced PFA model.

### Parameters
Same as `cyc`.

### Example

```lisp
(sx 'simple t
  (cyc2 'beat "bd ~ hats ~ sn ~ hats ~" :rep 60 :max-rep 3))
```

## `discourage` - Stir Up Generator

Looks at the last path through the graph and decreases the probablity for that sequence to happen again, effectively increasing entropy of the results. 

### Syntax

```lisp
(discourage <graph>)
```

### Example

```lisp
(sx 'chaos t
  (cyc 'gen "bd ~ ~ sn sn ~ casio ~" :rep 80 :rnd 80 :max-rep 4))
  
(grow 'gen :var 0.3) ;; execute a couple times

(discourage 'gen) ;; hear what happens
```

## `encourage` - Consolidate Generator

Looks at the last path through the graph and increases the probablity for that sequence to happen again, effectively decreasing entropy of the results. 

### Syntax

```lisp
(encourage <graph>)
```

### Example

```lisp
(sx 'chaos t
  (cyc 'gen "bd ~ ~ sn sn ~ casio ~" :rep 80 :rnd 80 :max-rep 4))
  
(grow 'gen :var 0.3) ;; execute a couple times

(encourage 'gen) ;; hear what happens
```

## `env` - Parameter Envelope

Define an envelope on any parameter. Length of list of levels must be one more than length of list of durations.

### Paramters

* levels (list) - level points on envelope path
* durations (list) - transition durations (in steps)
* `repeat` (boolean) - loop envelope 

### Syntax

```lisp
(env <levels> <durations> :repeat <t/nil>)
```

### Example

```lisp
(sx 'simple t
  (cmp (always (lvl (env '(0.0 0.4 0.0) '(20 30))))
       (cyc 'beat "bd ~ hats ~ sn ~ hats ~")))
```

## `evr` - Count-Based Generator Manipulators

## `exh` - Event Stream Manipulator

Exhibit event type, that is, mute all other events, with a certain probability.

### Parameters

* probablility (int) - exhibit probablility
* filter (filter function) - event type filter

### Syntax
```lisp
(exh <probability> <filter>)
```

### Example
```lisp
(sx 'simple t 
  (cmp (exh 30 hats)
       (exh 30 bd)
       (nuc 'beat (~ (bd) (sn) (hats)))))
```
## `fade` - Parameter Fader

## `grow` - Enlarge Generator

## `grown` - Enlarge Generator n times

## `haste` - speed up evaluation

## `inh` - Event Stream Manipulator

Inhibit event type, that is, mute event of that type, with a certain probability.

### Parameters

* probablility (int) - inhibit probablility
* filter (filter function) - event type filter

### Syntax

```lisp
(inh <probability> <filter>)
```

### Example

```lisp
(sx 'simple t
  (cmp (inh 30 hats)
       (inh 30 bd)
       (inh 30 sn)
       (nuc 'beat (~ (bd) (sn) (hats)))))
```

## `life` - Manipulate Generator

## `nuc` - Nucleus Generator

Generates a one-node repeating generator, i.e. as a starting point for growing.

### Parameters

* name (symbol)
* event(s) (event or list of events) - events to be repeated
* `:dur` - transition duration between events

### Syntax

```lisp
(nuc <name> <event(s)> :dur <duration>)
```

### Example

```lisp
(sx 'simple t
  (nuc 'beat (bd) :dur 400))
```

## `nuc2` - Nucleus Generator

Generates a one-node repeating generator, i.e. as a starting point for growing, based on PFA model.

Everything else same as `nuc`.

## `oscil` - Parameter Oscillator

Define oscillation on any parameter. The oscillation curve is a bit bouncy, not really sinusoidal.

### Parameters 

* upper limit - upper limit for oscillation 
* lower limit - lower limit for oscillation 
* `:cycle` - oscillation cycle length in steps

### Syntax

```lisp
(oscil <upper limit> <lower limit> :cycle <cycle length in steps>)
```

### Example

```lisp
(sx 'simple t
  (nuc2 'beat (bd) :dur (oscil 200 600 :steps 80)))
```

## `pear` - Apply Modifiers

Appl-ys and Pears ...

## `probctrl` - Manipulate Generator

## `prob` - Event Stream Manipulator Probablity

## `pprob` - Event Stream Manipulator Probablity

## `pseq` - Event Sequence Generated from Parameters

## `relax` - Slow Down Generator

## `shrink` - Shrink Generator

## `skip` - Skip Events

## `sx` - Event Sink

Allows for temporal independece of generators, unlike old `(s ...)`

### Example

```lisp
(sx 'simple t
  (nuc2 'beat (bd) :dur 400))

(sx 'simple2 t :sync 'simple :shift 200
  (nuc2 'beat2 (sn) :dur 400))
```

## `learn` - Learn Generator from Distribution

## `infer` - Infer Generator from Rules

## `stop` - Stop Event Processing

Stop event processing without deleting generators, thus maintaining current state.

## `xdup` - Multiply Generators Independently

## `xspread` - Multiply Generators Independently



