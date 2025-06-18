#!/usr/bin/env zsh
# William G. Scott/Wataru Kagawa/Gary Kerbaugh

# ---[ Load ]----------------------------------------------------------

autoload -U dirstack dirdump cpath chpwd

chpwd

# ---[ Limit Size ]----------------------------------------------------

# keep the zdirdump file from growing too long (250 entries seems ok)
tmp_timestamp=$(date | awk '{print $4}')

command touch $ZDOTDIR/cache/zdirdump
command cp $ZDOTDIR/cache/zdirdump /tmp/$tmp_timestamp.zdirdump
command tail -n 250 /tmp/$tmp_timestamp.zdirdump >| $ZDOTDIR/cache/zdirdump
command rm -f /tmp/$tmp_timestamp.zdirdump

# ---[ GUI ]-----------------------------------------------------------
typeset -U dirs_shared
dirstack $1 > /dev/null

# ---[ Alias Section ]-------------------------------------------------
alias dirstack='dirdump; typeset -U dirs_shared; dirstack'
alias cd\?='dirdump; typeset -U dirs_shared; dirstack'
# dvorak case
alias cdL="dirdump; typeset -U dirs_shared; dirstack" # as L is near ?
alias cdl="dirdump; typeset -U dirs_shared; dirstack" # easier to type than cd?
if [[ $(uname) == Darwin ]];then
  autoload -U _guidirs
  #_guidirs 2> /dev/null
  alias gdirs="dirdump; typeset -U dirs_shared; dirstack > /dev/null ; _guidirs"
  alias gd="dirdump; typeset -U dirs_shared; dirstack > /dev/null ; _guidirs"
fi

# ---[ Dump ]----------------------------------------------------------
##if [[ -f $ZDOTDIR/dirstack ]] && [[ ${#dirstack[*]} -eq 0 ]]; then
##  dirstack=( ${(uf)"$(< $ZDOTDIR/dirstack)"} )
##  echo "Loaded the dirstack from disk..."
##fi
##chpwd() { dirs -pl >! $ZDOTDIR/dirstack }

