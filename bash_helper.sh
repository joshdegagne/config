#!/bin/bash

# source ~/.git-prompt.sh # mac, gotta go get this online
. /usr/share/git-core/contrib/completion/git-prompt.sh # fedora has it!
PS1="[\u \w\$(__git_ps1 ' <%s>')]$ "

alias rebash='source ~/.bash_profile'

alias ls='ls -lhGAF'

alias cljtree='tree -FI *.class --prune' # tree that woks cleanly for clojure projects

alias ftp='ftp -i' # no interactive prompt

alias untargz='tar -zxvf'

function targz(){
    tar -zcvf $1.tar.gz $1
}

function ghead(){ # useful for peaking at csv.gz headers
    gzip -cd $1 | head
}

eval "$(thefuck --alias)"

alias q32='QHOME=~/q rlwrap -r ~/q/l32/q'

## erlang
export PATH=~/.cache/rebar3/bin:$PATH

## rust
export RUST_SRC_STABLE=/user/local/src/rust/stable/src
export RUST_SRC_BETA=/user/local/src/rust/beta/src
export RUST_SRC_NIGHTLY=/user/local/src/rust/nightly/src
export RUST_SRC_PATH=$RUST_SRC_BETA
export CARGO_HOME=~/.cargo
