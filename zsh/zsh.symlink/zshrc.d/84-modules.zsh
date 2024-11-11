# ---[ Modules ]-------------------------------------------------------
autoload -Uz compinit && compinit # check `zshrc.d/80-complete.zsh`
#zmodload zsh/complist
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -ap zsh/mapfile mapfile
autoload -U fold map filter # functional programming
autoload -U zmv # mv w/ pattern matching
autoload -U canonical_readlink # $(readlink -f) everywhere
autoload -U clearhis imv mkcdir
autoload -U fcd fcode fv
autoload -U vimgrep
