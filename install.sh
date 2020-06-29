#!/bin/sh

# emacs configuration
rm -rf ~/.emacs.d
rm -f ~/.emacs
ln -sf `pwd`/emacs.d ~/.emacs.d

# personal dictionary!
ln -sf `pwd`/aspell.en.pws ~/.aspell.en.pws

# kitty terminal emulator
ln -sf `pwd`/kitty.conf ~/.config/kitty/.

# clojure tooling
ln -sf `pwd`/profiles.clj ~/.lein/profiles.clj

# install shell configuration
# replace with fish?
# echo "source ~/config/bash_helper.sh" >> ~/.bashrc
# source ~/.bashrc
