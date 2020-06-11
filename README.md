# config

This config is built upon the shoulders of bloggers and coworkers (giants?)
There is an emacs config, a profiles.clj, and a few bash conveniences.

## emacs

The focus is to provide a configuration for development that includes
auto-completion, structural editing, and highlighting. Mouse-less navigation is a goal so various forms of jumping and searching are included

## requirements

### general

- __emacs__ 25 or greater
- __aspell__ for spell-checking
- __ripgrep__ for fs search

### programming language support

- __leiningen__ for clojure
- ...

## installation

````
./install.sh
````

Upon starting up Emacs for the first time, the third-party packages
will be automatically downloaded and installed.
