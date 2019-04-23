# Mégra Installation Guide

This is the new version of the Mégra installation guide, which attempts to give easier access to
the Mégra Language.

It focuses on using Mégra with **Portacle** (https://portacle.github.io), a self-contained Common Lisp
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
* graphviz
* git

Now, download **Portacle** from https://portacle.github.io and extract it to a location of your choice.

Download this file and put it into your portacle root folder (that is, the folder you just extracted):
https://github.com/the-drunk-coder/megra/blob/master/portacle-bootstrap-linux.sh

Run the bootstrap script, and you're about to be ready. In the Portacle root folder you should find the `megra` folder. In there, there's a file called `megra-supercollider-synths.scd`. Store the Synthdefs to you local system (using scide, for example). 

Now just run Portacle with the `portacle.run` script, and you'll end up directly in a Mégra file where you can start hacking around. Per default, the files are stored in the `megra-sketchbook` folder that lives in your Portacle folder now.

In the default installation, there's a few samples that'll be enough for the tutorial. Copy or link your samples into the `megra-samples` folder you'll find in the Portacle root folder. The structure is similar to *TidalCycles*, samples are organized in folders. 

## macOS

The installation process on macOS is fairly similar to the Linux process. Use *homebrew* or *MacPorts* to install the libraries as above. JACK doesn't work properly on macOS anymore, so there's no need to install it.

Here's a guide how to get Portacle running despite Apple's security theatre: https://portacle.github.io/#get-mac . It works! After you've been through the process, you can start Portacle like any other application from your Finder.

Then, use the macOS bootstrap script instead of the Linux one:
https://github.com/the-drunk-coder/megra/blob/master/portacle-bootstrap-macos.sh

The macOS version might not support MIDI right now, or at least it might have sync problems. Working on that. Not that MIDI support is really documented, anyway.

If you see an error "Division by Zero" on starting Portacle, make sure all samplerates of all programs are set to 48000 (or, if you've changed that in your ~/.incudinerc, the respective samplerate). It happens if there's a samplerate mismatch. 

## Windows
Now, I must admit that I don't stable access to a Windows machine, and I'm not well-versed anymore 
in operating it. I know that Portacle works on Windows (tried it on Windows 7), and so does SuperCollider.
SBCL thread support is needed by the underlying Incudine system, which is notoriously fragile on the Windows port.
Another problem is that you need to compile the several dependencies (like portaudio and portmidi)
by hand, so you'd need to be well-versed in Windows development. If you anybody is willing to try, please let
me know.
