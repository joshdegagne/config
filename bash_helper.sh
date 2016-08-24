
export RUST_SRC_STABLE=/user/local/src/rust/stable/src
export RUST_SRC_BETA=/user/local/src/rust/beta/src
export RUST_SRC_NIGHTLY=/user/local/src/rust/nightly/src
export RUST_SRC_PATH=$RUST_SRC_BETA
export CARGO_HOME=~/.cargo

source ~/.git-prompt.sh
PS1="< \u \w$(__git_ps1 ' [%s]') > "

alias ls='ls -GkFlAsh'
alias tree='tree -FI *.class --prune'
alias ftp='ftp -i'
alias untargz='tar -zxvf'
function targz(){
    tar -zcvf $1.tar.gz $1
}
