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

## `cyc` - Cycle Generator

## `cyc2` - Cycle Generator

## `discourage` - Stir Up Generator

## `e`, `edge` - Edge Constructor

## `encourage` - Consolidate Generator

## `env` - Parameter Envelope

## `exh` - Event Stream Manipulator

## `fade` - Parameter Fader

## `for` - Event Stream Selector

## `g`, `graph`  - Markov Graph Constructor

## `grow` - Enlarge Generator

## `grow2` - Enlarge Generator

## `inh` - Event Stream Manipulator

## `lifemodel` - Manipulate Generator 

## `n`, `node` - Node Constructor

## `nuc` - Nucleus Generator

## `nuc2` - Nucleus Generator

## `oscil` - Parameter Oscillator

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



