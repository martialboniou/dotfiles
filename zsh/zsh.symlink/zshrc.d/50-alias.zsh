# ---[ Alias Section ]-------------------------------------------------
alias c='clear'
alias l='ls -CF'
alias ssh='ssh -2'
alias pa='ps aux'
#alias vi='vim -c ":cd %:p:h"'
alias vv='fv'  # ie fzf -> nvim
alias mc='mc -bs'
alias man='PAGER=less man -a'
alias mv='nocorrect mv'       # no spelling correction on mv
alias cp='nocorrect cp'       # no spelling correction on cp
alias mkdir='nocorrect mkdir' # no spelling correction on mkdir
alias ln='nocorrect ln'       # no spelling correction on ln
alias touch='nocorrect touch' # no spelling correction on touch
alias jo=jobs
alias pu=pushd
alias po=popd
#alias d='dirs -v'
alias h=history
alias top=htop
alias stop='kill -TSTP' # csh like stop
alias grep=egrep #rg?
alias ll='ls -lah'
alias la='ls -a'
# List only directories and symbolic
# links that point to directories
alias lsd='ls -ld *(-/DN)'
# List only file beginning with "."
alias lsa='ls -ld .*'
# web cat
alias wcat='wget -q -O -'
alias dog=wcat
# less w/o double char
alias les=less
alias monitor="netstat | grep -v localhost | grep -v stream | grep -v dgram"
if [ "$MAN_COMMAND" ]; then
  alias man=$MAN_COMMAND
fi
if which elinks &> /dev/null; then
  alias html-w3m='elinks'
fi

# cd aliases (->hondana@gmx.com)
alias -- rh='cd'   # Dvorak keyboard: overuse of 'return' key
                   # may shift your hand from the row line
alias ..='cd ..'
alias ...='cd ../..'
alias cd..='cd ..'
alias cd.='cd ..'  # honestly 'cd .' is normally unused alone
alias cdc='cd ~; clear'
# zsh functions -- see modules
alias mcd='mkcdir' # AKA mkdir+cd
# NOTE: the following aliases require fd
alias ff='fcd'     # ie fzf <- cd .
alias ffc='fcode'  # ie fzf <- cd ~/Documents/Code
alias fcf='fcode'  # ie fzf <- cd ~/Documents/Code

# Dump specific
alias purge="rm -i *~" # emacs temp files to the void

# Suffixes (->hondana@gmx.com)
alias -s pdf=$OPEN_COMMAND
# `*-w3m` defined in 10-specific.zsh
alias -s html=html-w3m
alias -s org=org-w3m
alias -s {md,mkdn,markdown}=markdown-w3m

# Additional aliases
# pdf generation from markdown
## source: eugenkiss.com/blog/2011/fiction-in-markdown-with-pandoc
if (( $+commands[markdown2pdf] )); then # works with `cabal install pandoc`
  _a5_template="$HOME/.pandoc/templates/a5book.tex"
  alias makebookpdf='markdown2pdf --template=$_a5_template'
  alias makebookpdftoc='markdown2pdf --toc --template=$_a5_template'
  unset _as_template
fi
# autoload -U zsh-mime-setup
# zsh-mime-setup
alias sc="symfony console"
alias lua="luajit"
alias fennel="fennel-bin-luajit"
