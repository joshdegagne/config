#!/bin/sh

# emacs configuration
rm -rf ~/.emacs.d
rm -f ~/.emacs
ln -sf `pwd`/emacs ~/.emacs.d

# personal dictionary!
ln -sf `pwd`/aspell.en.pws ~/.aspell.en.pws

# alacritty terminal emulator
ln -sf `pwd`/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# clojure tooling
ln -sf `pwd`/clojure/leiningen/profiles.clj ~/.lein/profiles.clj

# elixir tooling
ln -sf `pwd`/elixir/i.ex ~/.iex.exs
