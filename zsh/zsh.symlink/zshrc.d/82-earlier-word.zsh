# ---[ Ctrl-J to paste earlier word ]-------------------------------------------
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey "^j" copy-earlier-word
