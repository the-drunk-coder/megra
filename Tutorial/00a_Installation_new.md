# Megra Installation Guide

This is the new version of the Megra installation guide, which attempts to give easier access to
the Megra Language.

It focuses on using Megra with **Portacle** (https:://portacle.github.io), a self-contained Common Lisp
Editor that already comes with all kinds of helpers. This is especially helpful for people who don't have
a lot of experience with the Common Lisp environment.

## Linux
I assume you have the following programs up and running: 

* JACK (jack2 preferred)
* SuperCollider
* sc3-plugins

If not, now is the time to use your distro's package manager to install them. While you're at it,
also install the following:

* libsndfile
* fftw
* portaudio 
* portmidi
* gsl
* git

Now, download **Portacle** from https://portacle.github.io and extract it to a location of your choice.

## macOS



## Windows
Now, I must admit that I don't stable access to a Windows machine, and I'm not well-versed anymore 
in operating it. I know that Portacle works on Windows (tried it on Windows 7), and so does SuperCollider.
I think the main problem is that you need to compile the several dependencies (like portaudio and portmidi)
by hand. 
