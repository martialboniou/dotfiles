# ---[ Completition system ]-------------------------------------------
zstyle ':completion:*' auto-description 'specify %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' squeeze-slashes true # remove trailing slash (useful for ln)
zstyle ':completion:*:cd:*' ignore-parents parent pwd # cd ../<tab> doesn't show parent
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' format '%d:'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' matcher-list '' '+m:{a-z}={A-Z} r:|[._-]=* r:|=*' '' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 2 not-numeric
zstyle ':completion:*' menu select=2 yes
zstyle ':completion:*' prompt 'Alternatives %e:'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' original true
zstyle ':completion:*' substitute 1
zstyle ':completion:*' verbose true
# grml.org/zsh/zsh-lovers::proxy for faster apt/port/dpkg
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/completion-cache
# grml.org/zsh/zsh-lovers::CVS uncompleted
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS'
# process ID
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

# use 'Ctrl-F' to complete menu
# http://chneukirchen.org/blog/archive/2013/03/10-fresh-zsh-tricks-you-may-not-know.html
zmodload zsh/complist
zle -C complete-menu menu-select _generic
_complete_menu()
{
  setopt localoptions alwayslastprompt
  zle complete-menu
}
zle -N _complete_menu
bindkey '^F' _complete_menu
bindkey -M menuselect '^F' accept-and-infer-next-history
bindkey -M menuselect '/' accept-and-infer-next-history
bindkey -M menuselect '^?' undo
bindkey -M menuselect ' ' accept-and-hold
bindkey -M menuselect '*' history-incremental-pattern-search-forward

