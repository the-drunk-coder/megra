# MEGRA INSTALLATION GUIDE

## DISCLAIMER
This guide is still on "expert" mode, like with so many
projects in their early stage, and probably incomplete.

I'm constantly working on making things more frictionless,
but i'm not there yet.

In the meanwhile, contact me, either per email (nik@parkellipsen.de)
	    or via the Livecoding slack channel (@parkellipsen),
	    and i'd be glad to help you making things work. 

## OS SUPPORT
This installation guide is based on linux, specifically Arch Linux.
Most programs you need will be available with other distributions, but
usually an older version (at least in the official repositories).

Also, i found arch quite well-suited for making music. Anyway, if you prefer
another distribution, you'll probably find anything you need in your favourite
package manager (including Homebrew if you're working with OSX).

I haven't worked with Windows in a while, i know that it's possible to compile
JACK and SBCL, but i've got no idea how they perform.

Currently, megra probably only works fluently with JACK (as the underlying
incudine library has some dependencies). The portaudio version hasn't been tested.
As JACK support on OSX isn't that good these days, i'm not too sure if this
will work well ... 

I assume you have a linux system up and running, and it's generally capable
of making sound.

## REQUIREMENTS

Now install:
- jack
- SuperCollider (> 3.7, higher recommended ... )
- git
- sbcl (1.3.19 or higher have been tested ... )
- emacs (25.2.x have been tested, currently the only interface)
- slime
- quicklisp

Clone the following repositories (ideally in your quicklisp/local-projects folder):
- incudine:       https://github.com/titola/incudine
- common music:   https://github.com/ormf/cm
- cm-incudine:    https://github.com/ormf/cm-incudine
- fudi-incudine:  https://github.com/ormf/fudi-incudine
- cm-fomus:       https://github.com/ormf/cm-fomus

## SETUP

Follow these instructions how to setup incudine (the underlying library for
sound and osc scheduling ... it's worth to have a look at!)

https://github.com/titola/incudine/blob/master/INSTALL

Update your quicklisp distributions (open a lisp file and
evaluate '(ql:update-all-dists)'

Now, in the file "megra-load.lisp" you'll find tow path, one points
to the folder containing the megra files, one to your samples (if
you want to use them ...). Samples are organized in a tidal-like
manner, in folders.

Add the following lines to your .emacs:

(push "<PATH-TO-MEGRA>" load-path)
(require 'incudine-megra)

