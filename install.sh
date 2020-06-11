#!/bin/sh
# DESTROY OLD EMACS!! (maybe this should just move themmm???)
rm -rf ~/.emacs.d
rm -f ~/.emacs
# install emacs configuration
ln -sf `pwd`/emacs.d ~/.emacs.d
# personal dictionary!
ln -sf `pwd`/aspell.en.pws ~/.aspell.en.pws
# install clojure tooling
ln -sf `pwd`/profiles.clj ~/.lein/profiles.clj
# install shell configuration
# replace with fish?
# echo "source ~/config/bash_helper.sh" >> ~/.bashrc
# source ~/.bashrc
