
source ~/.git-prompt.sh
PS1="< \u \w$(__git_ps1 ' [%s]') > "

alias ls='ls -GkFlAsh'
alias tree='tree -FI *.class --prune'
alias ftp='ftp -i'
alias untargz='tar -zxvf'
function targz(){
    tar -zcvf $1.tar.gz $1
}
