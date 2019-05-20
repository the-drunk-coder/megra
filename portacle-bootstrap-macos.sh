#!/bin/sh

# run this from your portacle root folder

# get megra repo
git clone https://github.com/the-drunk-coder/megra

# get dependencies
cd projects
git clone https://github.com/titola/incudine 
git clone https://github.com/ormf/cm 
git clone https://github.com/ormf/cm-incudine
git clone https://github.com/ormf/fudi-incudine
git clone https://github.com/ghollisjr/cl-libsndfile
git clone https://github.com/edicl/cl-fad
git clone https://gitlab.com/ellipsenpark/vom.git
cd ..

# copy tutorial samples
cp -R megra/Tutorial/tutorial_samples ./megra-samples

# copy incudinerc
cp megra/incudinerc_samples/incudinerc_osx ~/.incudinerc

# copy tutorial folder
cp -R megra/Tutorial ./megra-tutorial

# generate sketchbook folder
mkdir megra-sketchbook

# copy user.el
cp megra/portacle-user.el config/user.el

