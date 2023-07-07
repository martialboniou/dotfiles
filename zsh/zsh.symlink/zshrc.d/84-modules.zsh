# ---[ Modules ]-------------------------------------------------------
zmodload zsh/complist
autoload -Uz compinit
compinit
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -ap zsh/mapfile mapfile
autoload -U fold map filter # functional programming
autoload -U zmv # mv w/ pattern matching
autoload -U canonical_readlink # $(readlink -f) everywhere
autoload -U clearhis imv

