# Mégra Function Reference

In alphabetical order.

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
(s 'some ()
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
(s 'some ()
  (always (rate (brownian 0.8 1.2)))
  (nuc 'violin (violin 'a3)))
 ```

## `chain` - Create Event Processor Chain

Creates an event processor chain without dispatching it to sink.

### Parameters

* name - chain name
* generators - event generators

### Syntax

```lisp
(chain '<name> () 
  <generators>
)
```

### Example

```lisp
;; first define a chain
(chain 'some ()
  (always (rev 0.1))
  (nuc 'violin (violin 'a3)))
  
(s 'some ()) ;; dispatch to sink later  
```

## `clear` - Clear Session

Stops and deletes all present generators.

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
(chain 'some ()
  (always (rev 0.1))
  (nuc 'violin (violin 'a3)))

(chain 'more ()
  (always (rev 0.1))
  (nuc 'cello (cello 'c1)))

(s 'controller ()
  (g 'conductor ()
    (n 1 (ctrl #'(lambda () (stop 'more) (sink 'some ())))) ;; control function
    (n 2 (ctrl #'(lambda () (stop 'some) (sink 'more ()))))
    (e 1 2 :p 100 :d 3000)
    (e 2 1 :p 100 :d 3000)))

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
(s 'simple ()
  (cyc 'beat "bd ~ hats ~ sn ~ hats ~"))
```

## `cyc2` - Cycle Generator

Generates a cycle (aka loop) from a simple sequencing language, using the advanced PFA model. Currently doesn't have the `:rnd` parameter.

### Parameters

* name - generator name
* sequence - sequence description
* `:dur` - default space between events 
* `:rep` - probability of repeating an event
* `:max-rep` - limits number of repetitions

### Syntax

```lisp
(cyc2 <name> <sequence> :dur <duration> :rep <repetition probability> :max-rep <max number of repetitions>)
```

### Example

```lisp
(s 'simple ()
  (cyc2 'beat "bd ~ hats ~ sn ~ hats ~" :rep 60 :max-rep 3))
```

## `discourage` - Stir Up Generator

## `e`, `edge` - Edge Constructor

## `encourage` - Consolidate Generator

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
(s 'simple ()
  (always (lvl (env '(0.0 0.4 0.0) '(20 30))))
  (cyc 'beat "bd ~ hats ~ sn ~ hats ~"))
```

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
(s 'simple ()
  (exh 30 hats)
  (exh 30 bd)
  (nuc 'beat (~ (bd) (sn) (hats))))
```

## `fade` - Parameter Fader

## `for` - Event Stream Selector

## `g`, `graph`  - Markov Graph Constructor

## `grow` - Enlarge Generator

## `grow2` - Enlarge Generator

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
(s 'simple ()
  (inh 30 hats)
  (inh 30 bd)
  (inh 30 sn)
  (nuc 'beat (~ (bd) (sn) (hats))))
```

## `lifemodel` - Manipulate Generator 

## `n`, `node` - Node Constructor

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
(s 'simple ()
  (nuc 'beat (bd) :dur 400))
```

## `nuc2` - Nucleus Generator

Generates a one-node repeating generator, i.e. as a starting point for growing, based on PFA model.

### Parameters

* name (symbol)
* event(s) (event or list of events) - events to be repeated
* `:dur` - transition duration between events

### Syntax

```lisp
(nuc2 <name> <event(s)> :dur <duration>)
```

### Example

```lisp
(s 'simple ()
  (nuc2 'beat (bd) :dur 400))
```

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
(s 'simple ()
  (nuc2 'beat (bd) :dur (oscil 200 600 :steps 80)))
```

## `probctrl` - Manipulate Generator

## `prob` - Event Stream Manipulator Probablity

## `shrink` - Shrink Generator

## `s`, `sink` - Event Sink

Takes events and turns them into sound.

### Parameters:

* name (symbol)
* `:sync` (symbol) *optional*
* `:shift` (integer) *optional*

### Syntax:

```lisp
(s '<name> (:sync '<sync> :shift <milliseconds>) 
  <list of generators>
)
```

## `slearn` - Learn Generator from Distribution

## `sinfer` - Infer Generator from Rules

## `stop` - Stop Event Processing

Stop event processing without deleting generators, thus maintaining current state.



