# ---[ zshi interactive shell session  ]---------------------------------
if [[ $1 == eval ]]; then
  shift
  ICMD="$@"
  set --
  zle-line-init()
  {
    BUFFER="$ICMD"
    zle accept-line
    zle -D zle-line-init
  }
  zle -N zle-line-init
fi
