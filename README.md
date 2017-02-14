# config

This config is built on the shoulders of giants, and is slightly
reorganized to align with my taste. There is an emacs config, a
profiles.clj, and a few bash convinences.

## emacs

The focus is to provide a "batteries included" configuration for
development that includes auto-completion and integrated
documentation.

### override

In order to override parts of the configuration, create a directory
named the same as your user name in .emacs.d and add .el files
containing the overrides. All files in this directory will be loaded
after the rest of the configuration has been loaded.

## requirements

* emacs 24.4 or greater
* bash
* leiningen
* rustup

## installation

````
git clone git@gitlab.com:joshdegagne/config.git
ln -sf config/.emacs.d ~/emacs.d
ln -sf config/profiles.clj ~/.lein/profiles.clj
echo "source ~/config/bash_helper.sh" >> ~/.bashrc 
source ~/.bashrc
````

Upon starting up Emacs for the first time, the third-party packages
will be automatically downloaded and installed.

## references
The emacs config has been heavily inspired by:
* Achint Sandhu - https://github.com/sandhu/emacs.d
* Magnar Sveen - https://github.com/magnars/.emacs.d
* Steve Purcell - https://github.com/purcell/emacs.d
