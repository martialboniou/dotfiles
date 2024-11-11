# ---[ Completition system ]-------------------------------------------
# IMPORTANT: ^Xx : complete the aliases
# modules compinit/complist loaded in `zshrc.d/84-modules.zsh`
# (inspiration during this rework: https://github.com/Phantas0s/.dotfiles)
_comp_options+=(globdots)
setopt MENU_COMPLETE
setopt AUTO_LIST
setopt COMPLETE_IN_WORD
# no more _correct after _complete (restore at will)
# _expand_alias can be added before _complete too (but let's try `^Xx` for explicit completion)
zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/completion-cache
zstyle ':completion:*' complete true
zle -C alias-expansion complete-word _generic
bindkey '^Xx' alias-expansion # ^Xa is used for _increase-number (like in Vim; check ./96-increase-last-number.zsh)
zstyle ':completion:alias-expansion:*' completer _expand_alias
zstyle ':completion:*' insert-unambiguous false
zstyle ':completion:*' menu select=1 yes
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' complete-options true # options for cd instead of directory stack
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' squeeze-slashes true # remove trailing slash (useful for ln)
#zstyle ':completion:*' insert-unambiguous true
#zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*'
#'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' keep-prefix true
#zstyle ':completion:*' max-errors 2 not-numeric
#zstyle ':completion:*' prompt 'Alternatives %e:'
#zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
#zstyle ':completion:*' original true
#zstyle ':completion:*' substitute 1
# format (idea from https://thevaluable.dev/zsh-completion-guide-examples)
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %D %d --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*:*:*:*:messages' format ' %F{purple} -- %d -- %f'
zstyle ':completion:*:*:*:*:warnings' format ' %F{red} -- no matches found -- %f'
zstyle ':completion:*:*:*:*:default' list-colors ${(s.:.)LS_COLORS}
#zstyle ':completion:*' auto-description 'specify %d'
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories #ignore-parents parent pwd # cd ../<tab> doesn't show parent
#zstyle ':completion:*:*:kill:*' menu yes select
#zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*' group-name ''
zstyle ':completion:*:*:-command-:*:*' group-order aliases builtins functions commands
zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

# use 'Ctrl-F' to complete menu
# http://chneukirchen.org/blog/archive/2013/03/10-fresh-zsh-tricks-you-may-not-know.html
#zmodload zsh/complist
#zle -C complete-menu menu-select _generic
_complete_menu()
{
  setopt localoptions alwayslastprompt
  zle complete-menu
}
#zle -N _complete_menu
#bindkey '^F' _complete_menu
#bindkey -M menuselect '^F' accept-and-infer-next-history
#bindkey -M menuselect '/' accept-and-infer-next-history
#bindkey -M menuselect '^?' undo
#bindkey -M menuselect ' ' accept-and-hold
#bindkey -M menuselect '*' history-incremental-pattern-search-forward
