
source ~/.git-prompt.sh
PS1="< \u \w\$(__git_ps1 ' [%s]') > "

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
