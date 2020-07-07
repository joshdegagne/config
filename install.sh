#!/bin/sh

# emacs configuration
rm -rf ~/.emacs.d
rm -f ~/.emacs
ln -sf `pwd`/emacs ~/.emacs.d

# personal dictionary!
ln -sf `pwd`/aspell.en.pws ~/.aspell.en.pws

# kitty terminal emulator
ln -sf `pwd`/kitty/config.kitty ~/.config/kitty/kitty.conf

# fish - (f)riendly (i)nteractive (sh)ell
ln -sf `pwd`/fish/config.fish ~/.config/fish/conf.d/my_config.fish

# clojure tooling
ln -sf `pwd`/clojure/leiningen/profiles.clj ~/.lein/profiles.clj

# elixir tooling
ln -sf `pwd`/elixir/i.ex ~/.iex.exs
