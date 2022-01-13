#!/bin/sh

# emacs configuration
rm -rf ~/.emacs.d
rm -f ~/.emacs
ln -sf `pwd`/emacs ~/.emacs.d

# personal dictionary!
ln -sf `pwd`/aspell.en.pws ~/.aspell.en.pws

# alacritty configuration
mkdir -p ~/.config/alacritty
ln -sf `pwd`/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# zsh configuration
ln -sf `pwd`/.zshrc ~/.zshrc

# clojure tooling
ln -sf `pwd`/clojure/leiningen/profiles.clj ~/.lein/profiles.clj

# elixir tooling
ln -sf `pwd`/elixir/i.ex ~/.iex.exs
