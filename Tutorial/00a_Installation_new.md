# Mégra Installation Guide

This is the new version of the Mégra installation guide, which attempts to give easier access to
the Mégra Language.

It focuses on using Mégra with **Portacle** (https://portacle.github.io), a self-contained Common Lisp
Editor that already comes with all kinds of helpers. This is especially helpful for people who don't have
a lot of experience with the Common Lisp environment.

This guide assumes that you've some basic experience with handling a terminal. If you don't, here's some hints
on how to familiarize yourself with it:
* http://linuxcommand.org/lc3_learning_the_shell.php (general)
* https://macpaw.com/how-to/use-terminal-on-mac (macOS specific)

**WARNING**: This guide prompts you to download all kinds of scripts from the internet. It's recommendable
to do at least a sanity check before executing them!

## Table of Contents:

* [Linux](#Linux) installation instructions
* [macOS](#macOS) installation instructions
* [Windows](#Windows) installation hints
* [Samples](#Samples) tips on how to manage a sample set for Mégra 

## Linux
Make sure you have the following programs up and running: 

* JACK (jack2 preferred)
* SuperCollider (3.8 or later)
* sc3-plugins
* gcc

If not, now is the time to use your distro's package manager to install them. While you're at it,
also install the following:

* libsndfile
* fftw
* portaudio 
* portmidi
* gsl
* graphviz
* git
* wget

Now, download **Portacle** from https://portacle.github.io and extract it to a location of your choice (I'd recommend your home folder). **NOTE:** if you already have portacle in use, I heavily recommend using a separate installation dedicated to Mégra.

Open a terminal. In the terminal, navigate to the portacle folder (that is, the folder you just extracted) and execute the following line. This will download the installation script:

```wget https://raw.githubusercontent.com/the-drunk-coder/megra/master/portacle-bootstrap-linux.sh```

Now make the installation exectuable and run it. This will download and install all the necessary Lisp code and Lisp dependencies for Mégra.

```chmod +x portacle-boostrap-linux.sh && ./portacle-boostrap-linux.sh```

Now just run Portacle by executing the `portacle.run` script in the portacle folder:

```./portacle.run```

You'll end up directly in a Mégra file where you can start hacking around. Per default, the files are stored in the `megra-sketchbook` folder that lives in your portacle folder now.

You're ready to follow the tutorial now. You can find the tutorial in the `megra-tutorial` folder that also lives inside your portacle folder.

## macOS

Mégra runs on macOS from version 10.11 onwards, even though I'd recommend at least 10.12, the oldest supported version as of 2019.

Make sure you have the following programs up and running: 

* SuperCollider (3.8 or later)
* sc3-plugins

If not, you can find those here:
* SuperCollider: https://supercollider.github.io/download
* sc3-plugins: https://supercollider.github.io/sc3-plugins

You need a C/C++ compiler to install certain libraries (don't worry, you don't have to interact with it in any way).
To check whether you have one already, open a terminal, type `cc` and press `Return`. If you see something along the lines of
`clang: error: no input files found`, you should be all set.

If not, enter `xcode-select --install` in the terminal to install the necessary command line tools. For more detailed
instructions, see: https://developpaper.com/install-command-line-tools-no-xcode-in-mac-os-x/.

Now make sure you have the following packages installed:
* libsndfile
* fftw
* portaudio 
* portmidi
* gsl
* graphviz
* git
* wget

I recommend **Homebrew** to install them. If you do not have homebrew, here's instructions on how to get it: https://brew.sh .
With homebrew installed, just open a terminal and type:

```brew install libsndfile fftw portaudio portmidi gsl graphviz git wget```

If you have some other package manager like *macPorts* installed, you might use that as well, in which case I assume you know what you're doing.

Now you need to get portacle. Here's a guide how to get Portacle running despite Apple's security theatre: https://portacle.github.io/#get-mac . It works! After you've been through the process, you can start Portacle like any other application from your *Finder*. As a hint, do NOT put portacle in your global *Applications* folder, instead just move the *portacle* folder from the DMG to your home folder, or Desktop if you want. **NOTE:** if you already have portacle in use, I heavily recommend using a separate installation dedicated to Mégra.

Then, download the installation script by navigating to the portacle folder in the terminal and execute the following line: 

```wget https://raw.githubusercontent.com/the-drunk-coder/megra/master/portacle-bootstrap-macos.sh```

Now make the installation exectuable and run it. This will download and install all the necessary Lisp code and Lisp dependencies for Mégra.

```chmod +x portacle-boostrap-macos.sh && ./portacle-boostrap-macos.sh```

Now, if you start portacle, you'll end up directly in a Mégra file where you can start hacking around. Per default, the files are stored in the `megra-sketchbook` folder that lives in your portacle folder now.

You're ready to follow the tutorial. You can find the tutorial in the `megra-tutorial` folder that lives inside your portacle folder now.

### Troubleshooting

If you see an error "Division by Zero" while Portacle is starting and loading the Mégra system, make sure all samplerates of all programs are set to 48000 (or, if you've changed that in your ~/.incudinerc, the respective samplerate). It happens if there's a samplerate mismatch. If you have another audio application running (i.e. Reaper), it might change the samplerate if you pull that window into focus !

## Windows
Now, I must admit that I don't have stable access to a Windows machine, and I'm not well-versed anymore 
in operating one. I know that Portacle works on Windows (tried it on Windows 7), and so does SuperCollider.
SBCL thread support is needed by the underlying Incudine system, which is notoriously fragile on the Windows port.
Another problem is that you need to compile the several dependencies (like portaudio and portmidi)
by hand, so you'd need to be well-versed in Windows development. If you anybody is willing to try, please let
me know.

# Samples
In the default installation, there's a very basic set of samples that'll be enough for the tutorial. They can be found in the `megra-samples` folder that is inside portacle folder. The structure is similar to *TidalCycles*, samples are organized in folders. The samples are in **FLAC** format and **mono**.

When you start Mégra, it'll check for sub-folders in the `megra-samples` folder and create an event for each of those.

If you want to add samples, create a subfolder in `megra-samples` and put your samples there.
