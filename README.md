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

- __kitty__ graphics accelerated terminal emulation
- __thefuck__ cute way to fix mistakes

### programming language support

#### clojure

- __java__ jvm runtime
- __leiningen__ cider, project configuration, package management

#### erlang/elixir

- __erlang/OTP__ BEAM runtime
- __rebar3__ erlang project configuration, package management
- __elixir__ all the elixir tooling
- __elixir-lsp__ integration with emacs

### fonts

- [Fira Code](https://github.com/tonsky/FiraCode)
- [Fira Code Symbols](https://github.com/Profpatsch/blog/blob/master/posts/ligature-emulation-in-emacs/post.md#appendix-b-update-1-firacode-integration) (for emacs)

## installation

```bash
./install.sh
```

Upon starting up Emacs for the first time, the third-party packages
will be automatically downloaded and installed.
