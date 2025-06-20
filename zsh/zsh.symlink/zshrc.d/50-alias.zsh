# ---[ Alias Section ]-------------------------------------------------
alias rgl="rg -g '!*/'" # exclude subdirectories (l = local)
alias c='clear'
alias pa='ps aux'
#alias vi='vim -c ":cd %:p:h"'
alias vv='fv'  # ie fzf -> nvim
alias mc='mc -bs'
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
alias la='ls -a'
# List only directories and symbolic
# links that point to directories
alias lsld='ls -ld *(-/DN)' # NOTE lsld was lsd but https://github.com/lsd-rs/lsd
# List only file beginning with "."
alias lsla='ls -ld .*'
# web cat
alias wcat='wget -q -O -'
alias dog=wcat
# less w/o double char
alias les=less
alias monitor="netstat | grep -v localhost | grep -v stream | grep -v dgram"
if [ "$MAN_COMMAND" ]; then
  alias man=$MAN_COMMAND
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
alias ff='fcode' # ie fzf <- cd ~/Documents/Code or any $DEVELOPER_ROOT; might be renamed on AIX

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
declare -A app_aliases
app_aliases=( vim nvim awk gawk grep ggrep html elinks )
for k v in ${(kv)app_aliases}; do
  if (( $+commands[$v] )); then
    alias $k="${v}"
  fi
done
# eza
if (( $+commands[eza] )); then
  alias ls='eza --long -s=newest --group-directories-first --git --sort=modified'
  alias l='$aliases[ls]' # -C won't work in eza
  alias ll='$aliases[ls] -a'
  # `la` for the hidden files/folders too
else
  alias l='ls -CF'
  alias ll='ls -lah'
fi
# markdown2pdf
if (( $+commands[markdown2pdf] )); then # works with `cabal install pandoc`
  _a5_template="$HOME/.pandoc/templates/a5book.tex"
  alias makebookpdf='markdown2pdf --template=$_a5_template'
  alias makebookpdftoc='markdown2pdf --toc --template=$_a5_template'
  unset _as_template
fi
# autoload -U zsh-mime-setup
# zsh-mime-setup
alias sc="symfony console"
#
# weather in my location (change at will)
alias weather+='curl -s v2.wttr.in/Brest'
zmodload -F zsh/parameter p:aliases
alias weather='curl -s wttr.in/Brest'
alias amzer='$aliases[weather]'
alias meteo='$aliases[weather]'
# dF = df summary on macOS
if [[ $(uname) == Darwin ]]; then
  # alias dF="df -g | awk '/\/(Data|nix|)$/ {used+=\$3;avail+=\$4} END{print \"Used: \"used\"G\t Available: \"avail\"G\"}'"
  alias dF='diskutil list'
fi
