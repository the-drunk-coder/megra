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

cd megra/sc_synths
sclang megra-supercollider-synths-2ch.scd
sclang megra-supercollider-synths-4ch.scd
sclang megra-supercollider-synths-8ch.scd
cd ../..

# copy tutorial samples
git clone https://github.com/the-drunk-coder/megra-public-samples megra-samples

# copy incudinerc
cp megra/incudinerc_samples/incudinerc_linux ~/.incudinerc

# copy tutorial folder
cp -R megra/Tutorial ./megra-tutorial
cp -R megra/vis ./vis

# generate sketchbook folder
mkdir megra-sketchbook

# copy user.el
cp megra/portacle-user.el config/user.el
