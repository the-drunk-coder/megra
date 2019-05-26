# Mégra Tutoral, Part 1 - Creating Structures

Welcome to the Mégra tutorial ! Assuming you've got everything up and running, we can start to make some sound !

Make sure your headphones are not too lound. 

Now, type the following lines of code in Portacle (or copy/paste them), move the curser to somewhere in between the outer
parenthesis and hit `C-<return>`:

```lisp
(s 'first ()
	(graph 'beep ()
		(node 1 (sine 'a4 :lvl 0.5 :dur 500))
		(edge 1 1 :dur 1000 :prob 100)))
```
If nothing went wrong, you should hear a steady beeping now. This means we're ready to start.

```lisp
(stop) ;; Silence ! 
```

Now that we know we can make sound, here's some basic things about Mégra. 
1. Mégra is mostly suited to create musical structures. It has some synthesis aspects, but that's currently not its strongest side
2. Mégra uses *Variable-Order Markov Chains* as its basic data structure. We'll explain that later on.
3. Mégra also uses *Probabilistic Finite Automata*, which is a specific representation of *Variable Order Markov Chains*

## 1.1 Markov Chains
If you're into generative music, you probably know already what a *Markov Chain* is, as it is a fairly common structure in that domain. 
If not, here's a simple explanation. Even if you know them already, i'd recommend reading the following part to get to know the specific 
"flavour" of Markov Chains employed by Mégra.

Imagine you want to create a simple *boom-bap* style beat, with just a bassdrum, a snare and some hi-hats. 
Only half the time the hi-hat should be in between the bassdrum and the snare. 

In slightly more mathematical terms, we could describe that the following with some rules. The natural language description
is a bit tedious, but bear with me here. More concise descriptions will follow! 

1. Start with a bassdrum
2. There's a 50% chance that a snare will follow the bassdrum
3. There's a 50% chance that a hi-hat will follow the bassdrum
4. After every snare will follow a bassdrum
5. After every hi-hat will follow a snare

This could be easily represented graphically:

This representation is called a *directed graph* (as each arrow has a direction), that has a couple of *nodes* and *edges* that 
connect the nodes. 

Now, Mégra has a fairly direct way to create sequence generators based on Markov chains. A *generator* is something that emits 
musical *events*. 

```lisp
(sink 'boom () ;; <- this turns the events into actual sounds. More about that later! 
	(graph 'bap () <- this creates the generator
		(node 1 (bd)) ;; <- each node has a number and a list of associated events 
		(node 2 (sn))
		(node 3 (hats))
		(edge 1 2 :prob 50 :dur 400) ;; <- duration in milliseconds
		(edge 1 3 :prob 50 :dur 200) 
		(edge 2 1 :prob 100 :dur 400)
		(edge 3 1 :prob 100 :dur 400)))
```

As you can see, each edge not only has a probability, but also a duration that determines the time between events. This time 
is specified in milliseconds. Play around with the probablities and durations a bit to hear how the result changes! Make sure 
that the sum of outgoing probabilities for each node is 100% ! If not, it'll still work but it might be harder to explain the 
results. 

If you look at the structure above, each event is only dependent on the immediately preceding event. In technical terms,
that's what we call a *first-order* Markov chain. Now, earlier there's been mentioned something about *Variable Order Markov Chains*. 


## 1.2 Writing Generators by Hand 

## 1.3 Generating Generators


