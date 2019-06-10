# Mégra Tutorial, Chapter 3 - Controlling Sound

In the last chapters, the main topic was creating generators for musical structures. Now we will look into how to 
control sound parameters with Mégra.

A list of all sound events available at the current time, plus a list of all their parameters, can be found in **Appendix A**.

* 3.1 - [Sample Sounds](#31-sample-sounds)
* 3.2 - [Event Streaming](#32-detour-event-streaming)
* 3.3 - [Synth Sounds](#33-synth-sounds)
* 3.4 - [Technique: Inhibition and Exhibition](#34-technique-event-inhibition-and-exhibition)

## 3.1 Sample Sounds
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

## 3.2 Detour: Event Streaming

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
The `(always ...)` operator means that those modifications will always be applied. We can also apply them with a certain probablity. Say you want a 30% chance that the sound will be reverberated ... the `(prob ...)` function will help you:

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

If you only want to apply a modifier to one specific type of sound event, you can filter the event stream for that with the `(for ..)` function:

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


## 3.3 Synth Sounds

The principal synth sounds that Mégra currently offers are simple one-oscillator sounds. The following example is a simple, pulsing squarewave bass sound:

```lisp
(s 'bass ()
  (nuc 'sqare (sqr 90 :lp-dist 1.0 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5) :dur 200))
```

The `(sqr ...)` function will produce that type of sound event. Just to remind you, another way to write this would be:

```lisp
(s 'bass ()
  (always (lp-dist 1.0) (lp-freq 1000) (atk 1) (rel 99) (dur 100) (lvl 0.5))
  (nuc 'sqare (sqr 90) :dur 200))
```

Those two variants will produce the same sound. 

It works the same with other types of waves:

* Sawtooth:
```lisp
(s 'bass ()
  (nuc 'wave (saw 90 :lp-dist 1.0 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5) :dur 200))
```

* Triangle:
```lisp
(s 'bass ()
  (nuc 'wave (tri 90 :lp-dist 1.0 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5) :dur 200))
```

* Different sine waves:
```lisp
(s 'bass () ;; regular sine
  (nuc 'wave (sine 90 :lp-dist 1.0 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5) :dur 200))
  
(s 'bass () ;; LFCub sine
  (nuc 'wave (cub 90 :lp-dist 1.0 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5) :dur 200))
  
(s 'bass () ;; LFPar sine
  (nuc 'wave (par 90 :lp-dist 1.0 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5) :dur 200))
```

## 3.4 Technique: Event Inhibition and Exhibition

One nice technique based on the event streaming idea is the idea of event inhibition and exhibition. Look at the following structure:

```lisp
(s 'all-in ()
  (nuc 'one (list (saw 90 :lp-dist 1.0 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5)
                  (bd 'boom)
                  (sn 'tschack))
       :dur 100))
```
A bit massive, isn't it ? Now, would't it be great if we could *inhibit* the snar occasionally, say, with a 30% chance? 
Here we go:


```lisp
(s 'all-in ()
  (inh 30 sn) ;; <- the inhibitor! 
  (nuc 'one (list (saw 90 :lp-dist 1.0 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5)
                  (bd 'boom)
                  (sn 'tschack))
       :dur 100))
```
That seems to make things a bit more interesting, but how about this one:

```lisp
(s 'all-in ()
  (inh 30 sn) ;; <- the inhibitor!
  (inh 30 bd)
  (inh 30 saw)
  (nuc 'one (list (saw 90 :lp-dist 1.0 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5)
                  (bd 'boom)
                  (sn 'tschack))
       :dur 100))
```
That already creates quite a complex rhythm. 

There's also the reciprocal function, `(exh ...)`, which inhibits all event types *except* the specified one:

```lisp
(s 'all-in ()
  (exh 30 sn) ;; <- the exhibitor!
  (exh 30 bd)
  (exh 30 saw)
  (nuc 'one (list (saw 90 :lp-dist 1.0 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5)
                  (bd 'boom)
                  (sn 'tschack))
       :dur 100))
```

Mixing the two, you can create complex rhythms from a single nucleus.


