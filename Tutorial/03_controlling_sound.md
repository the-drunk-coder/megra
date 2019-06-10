# Mégra Tutorial, Chapter 3 - Controlling Sound

In the last chapters, the main topic was creating generators for musical structures. Now we will look into how to 
control sound parameters with Mégra.

## Sample Sounds, Part 1

### Choosing Samples

In the last chapter, we created beats from samples. You might have noticed that the sound changes sometimes when you re-run the code. Before we get deeper into that, first take a look into the `megra-samples` folder inside your Portacle folder. You can see different subfolders named `bd`, `sn`, etc. that look just like the events we use to fill our structures. And in fact, when Mégra is started, it scans through the `megra-samples` folder and creates one event representation for each of the subfolders. Now, when the event is called "as is", Mégra will just choose a random sample from that subfolder.

```lisp
;; execute multiple times and see how it changes
(s 'ran ()
  (nuc 'samp (bd) :dur 400))
```

Now, is there a more controlled way ? Yes there is ! You can provide one or more keywords to pick a sample. If you run the following code multiple times, the sound shouldn't change:

```lisp
(s 'ran ()
  (nuc 'samp (bd 'bd808) :dur 400)) ;; look through the "bd"  folder, change the keyword, and see what happens!
```
So, now browse through the samples folder a bit and see what you can find! If you provide your own samples, make sure they are named in a human-understandable manner (unless you like the surprises, like me)! The keywords do **not** have to match the actual filename! Also, you can provide **more than one** keyword. That gives you quite some control over how much precision you want.

### Sample Event Parameters
Just choosing the sample sounds would be boring without being able to choose the parameters, right ? So, here's the way you can control parameters in Mégra. Let's say you want to add some reverb ... here you go:

```lisp
(s 'ran ()
  (nuc 'samp (sn 'fat :rev 0.1) :dur 800))
```
Or some lowpass filter:

```lisp
(s 'ran ()
  (nuc 'samp (sn 'fat :rev 0.01 :lp-freq 700) :dur 800))
```

You can even add operations on the filters (I'll leave the details on that for later):

```lisp
(s 'ran ()
  (nuc 'samp (sn 'fat :rev 0.01 :lp-freq (oscil 100 7000)) :dur 800))
```

For a full list of the parameters that each sample event provides, take a look at the appendix!

Now, take a look at the beat generators we saw in the last chapter:

```lisp
(s 'a-basic ()
  (cyc 'beat "[hats bd] ~ ~ ~ [sn hats] ~ ~ ~" :dur 100))
```

There doesn't seem to be room to provide the parameters, right ? Well, that's only partly right. You can provide the keywords like so:

```lisp
(s 'a-basic ()
  (cyc 'beat "[hats:'accent bd:'boom] ~ ~ ~ [sn:'tschack hats:'hh] ~ ~ ~" :dur 100))
```
For the sound parameters, we need to take a little detour first ...

## Detour: Event Streaming

Mégra is all about events. Events, like a bassdrum or snare event, travel from an event generator, or source, towards an event sink, like you can see in the example below.

```lisp
(s 'strings () ;; <- this is an event sink
  ;; ^
  ;; |
  ;; | Events are streamed in this direction ... there's a lot of things that can happen !
  ;; |
  (cyc 'vio "violin:'a3 ~ ~ ~ violin:'a4 ~ ~ ~" :dur 300)) ;; <- this is an event source
```
On their way, there's a lot of things that can happen to them. for example, we can modify the duration, attack, and release time:

```lisp
(s 'strings () ;; <- this is an event sink
  ;; ^
  ;; |
  ;; | 
  (always (dur 150) (atk 2) (rel 100))  ;; here's the modifier
  (cyc 'vio "violin:'a3 ~ ~ ~ violin:'a4 ~ ~ ~" :dur 300)) ;; <- this is an event source
```
The `always` operator means that those modifications will always be applied. We can also apply them with a certain probablity. Say you want a 30% chance that the sound will be reverberated:

```lisp
(s 'strings () ;; <- this is an event sink
  ;; ^
  ;; |
  (prob 30 (rev 0.2)) ;; <- here's your occasional reverb (on all events)
  (always (dur 150) (atk 2) (rel 100))  
  (cyc 'vio "violin:'a3 ~ ~ ~ violin:'a4 ~ ~ ~" :dur 300)) ;; <- this is an event source
```
You can even mix in other events from another generator:

```lisp
(s 'strings () ;; <- this is an event sink
  ;; ^
  ;; |
  (prob 30 (rev 0.2)) ;; <- here's your occasional reverb ...
  (always (dur 150) (atk 2) (rel 100))  
  (cyc 'cel "~ cello:'c1 ~ ~ ~ ~ cello:'c3 ~") ;; some cello?
  (cyc 'vio "violin:'a3 ~ ~ ~ violin:'a4 ~ ~ ~" :dur 300)) ;; this one still controls the timing!
```

In the case above, the first generator (violin) controls the timing, and whenever the second one receives an event,
it'll mix in its own!

If you only want to apply a modifier to one specific sound, you can filter the event stream for that with the `(for ..)` 
function:

```lisp
(s 'strings () ;; <- this is an event sink
  ;; ^
  ;; |
  (prob 30 (rev 0.2)) ;; <- here's your occasional reverb (on all events)
  (for violin (always (dur 150) (atk 2) (rel 100))) ;; <- only apply this modifier to the violin events ...
  (cyc 'cel "~ cello:'c1 ~ ~ ~ ~ cello:'c3 ~") ;; some cello?
  (cyc 'vio "violin:'a3 ~ ~ ~ violin:'a4 ~ ~ ~" :dur 300))
```

And finally, you can also add modifiers to the modifiers (phew ...):

```lisp
(s 'strings () ;; <- this is an event sink
  ;; ^
  ;; |
  (prob 30 (rev 0.2)) ;; <- here's your occasional reverb ...
  (for violin (always (dur 150))) ;; <- only apply this modifier to the violin events ...
  (always (filter-lp (oscil 400 3000)))
  (cyc 'cel "~ cello:'c1  ~ cello:'c3 ~") 
  (cyc 'vio "violin:'a3 ~ ~ violin:'a4 ~ " :dur 200))
```

So far we have only covered sample sounds, but Mégra has some (admittedly basic) synth sounds as well, as we'll see in the next section.


## Synth Sounds
