# grml.org/zsh/zsh-lovers::quick change directories
# (cd ..../dir) -> cd ../../../dir
rationalize-dot(){
 if [[ $LBUFFER = *.. ]]; then
  LBUFFER+=/..
 else
  LBUFFER+=.
 fi
}
zle -N rationalize-dot
bindkey . rationalize-dot
