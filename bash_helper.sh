
source ~/.git-prompt.sh
PS1="< \u \w$(__git_ps1 ' [%s]') > "

alias ls='ls -GkFlAsh'
alias tree='tree -FI *.class --prune' # tree that woks cleanly for clojure projects
alias ftp='ftp -i'
alias untargz='tar -zxvf'
function targz(){
    tar -zcvf $1.tar.gz $1
}
function ghead(){ # useful for peaking at csv.gz headers
    gzip -cd $1 | head
}

alias q32='QHOME=~/q rlwrap -r ~/q/l32/q'
