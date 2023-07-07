#!/usr/bin/env zsh
# 'Ctrl-X A' increases the last number
_increase_number()
{
  local -a match mbegin mend
  [[ $BUFFER =~ '([0-9]+)[^0-9]*$' ]] && LBUFFER[mbegin,mend]=$(printf %0${#match[1]}d $((10#$match+${NUMERIC:-1})))
}
zle -N increase-number _increase_number
bindkey '^Xa' increase-number
bindkey -s '^Xx' '^[-^Xa'
