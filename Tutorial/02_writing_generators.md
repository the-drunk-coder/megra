# Mégra Tutorial, Chapter 2 - Writing Generators

As we've seen in the last chapter, Mégra is all about *sequence generators*. There's 
three principal ways to write *sequence generators* in Mégra:

1. Writing generators by hand.
2. Generating generators.
3. Learing generators.

## 2.1 Writing Generators by Hand 
This is the method we've already encountered in the previous chapter. It's a bit tedious, but it's
a good way to become familiar with the model. There's also some shorthands. Let's look again at
the beat generator from the last chapter:

```lisp
(sink 'boom () ;; <- this turns the events into actual sounds. More about that later! 
	(graph 'bap () ;; <- this creates the generator
		(node 1 (bd)) ;; <- each node has a number and a list of associated events 
		(node 2 (sn))
		(node 3 (hats))
		(edge 1 1 :prob 80 :dur 400) 		
		(edge 1 2 :prob 10 :dur 400) 
		(edge 1 3 :prob 10 :dur 200) 
		(edge '(1 1 1 1) 3 :prob 70 :dur 200) 
		(edge '(1 1 1 1) 2 :prob 30 :dur 200) 
		(edge 2 1 :prob 100 :dur 400)
		(edge 3 1 :prob 100 :dur 400)))
```

This can be shortened to:

```lisp
(s 'boom () ;; sink becomes s
	(g 'bap ()  ;; graph becomes g
		(n 1 (bd)) ;; node becomes n
		(n 2 (sn))
		(n 3 (hats))
		(e 1 1 :p 80 :d 400) ;; edge becomes e, prob becomes p, dur becomes d
		(e 1 2 :p 10 :d 400) 
		(e 1 3 :p 10 :d 200) 
		(e '(1 1 1 1) 3 :p 70 :d 200) 
		(e '(1 1 1 1) 2 :p 30 :d 200) 
		(e 2 1 :p 100 :d 400)
		(e 3 1 :p 100 :d 400)))
```

That's a bit shorter, but still a lot of lines to write before you can hear the first sound. Luckily, there's 
some shorter methods here ! 

## 2.2 Generating Generators

We can save lots of lines of code by generate the generators from a more abstract description, or we can start out from
a very simple, repeated event and "grow" a generator by repeatedly adding new nodes.

Let's look at the second method first ! 

## 2.3 Learning Generators

Now we're arriving in 2019, where machine learning is all the rage! 

