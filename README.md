# config

This config is built upon the shoulders of bloggers and coworkers (giants?)
There is an emacs config, a profiles.clj, and a few bash conveniences.

## emacs

The focus is to provide a configuration for development that includes
auto-completion, structural editing, and highlighting. Mouse-less navigation is a goal so various forms of jumping and searching are included

## requirements

### text editing

- __emacs__ 25 or greater
- __aspell__ spell-checking
- __ripgrep__ file search

### terminal

- __alacritty__ graphics accelerated terminal emulation
- __thefuck__ cute way to fix mistakes
- __zoxide__ & __fzf__ easier cd'ing when there are lots of long paths

### programming language support

#### clojure

- __java__ jvm runtime
  ```zsh
  brew install openjdk
  ```
    - __jenv__ for managing versions
      ```zsh
      brew install jenv
      jenv add /usr/local/opt/openjdk/libexec/openjdk.jdk/Contents/Home
      ```
- __leiningen__ cider, project configuration, package management

#### erlang/elixir

- __erlang/OTP__ BEAM runtime
- __rebar3__ erlang project configuration, package management
- __elixir__ all the elixir tooling
- __elixir-lsp__ integration with emacs

#### golang

stuff!?!?!

### font

- brew install font-monofur-nerd-font

## installation

```bash
./install.sh
```

Upon starting up Emacs for the first time, the third-party packages
will be automatically downloaded and installed.
