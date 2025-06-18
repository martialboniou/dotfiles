# ---[ Completition system ]-------------------------------------------
# IMPORTANT: ^Xx : complete the aliases
# modules compinit/complist loaded in `zshrc.d/84-modules.zsh`
# (inspiration during this rework: https://github.com/Phantas0s/.dotfiles)
_comp_options+=(globdots)
setopt MENU_COMPLETE
setopt AUTO_LIST
setopt COMPLETE_IN_WORD
setopt NO_ALWAYS_LAST_PROMPT # IMPORTANT: check complist code below
# no more _correct after _complete (restore at will)
# _expand_alias can be added before _complete too (but let's try `^Xx` for explicit completion)
zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' accept-exact false # IMPORTANT! don't accept the match on <TAB>
#
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/completion-cache
# interactive means no <TAB> in menu until first <RET> or <SPACE>
zstyle ':completion:*' menu select=1 # interactive # (IMO: bad option; instead get the good pattern first)
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' complete-options true # options for cd instead of directory stack
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' squeeze-slashes true # remove trailing slash (useful for ln)
# zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*'
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*' # 'l:|=* r:|=*'
zstyle ':completion:*' keep-prefix true
#
# format (idea from https://thevaluable.dev/zsh-completion-guide-examples)
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %D %d --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*:*:*:*:messages' format ' %F{purple} -- %d -- %f'
zstyle ':completion:*:*:*:*:warnings' format ' %F{red} -- no matches found -- %f'
zstyle ':completion:*:*:*:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors 'ma=48;2;76;86;106' # gray selection, red match
zstyle ':completion:*' group-name ''
#
# expand alias on <CTRL>+X followed by X
zle -C alias-expansion complete-word _generic
bindkey '^Xx' alias-expansion # ^Xa is used for _increase-number (like in Vim; check ./96-increase-last-number.zsh)
zstyle ':completion:alias-expansion:*' completer _expand_alias
#
zstyle ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )' # or 2
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories #ignore-parents parent pwd # cd ../<tab> doesn't show parent
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
# idea: aliases are short and well-known so no need to have them first IF accept-exact is TRUE (reminder if switching back)
zstyle ':completion:*:*:-command-:*:*' group-order local-directories aliases builtins functions commands # (IMO: best option)
zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'
#
# use 'Ctrl-N' to complete menu (it was Ctrl-F originally)
# http://chneukirchen.org/blog/archive/2013/03/10-fresh-zsh-tricks-you-may-not-know.html
zmodload zsh/complist
zle -C complete-menu-generic menu-select _generic
_complete_menu()
{
  setopt localoptions alwayslastprompt
  zle complete-menu-generic
}
zle -N complete-menu _complete_menu
bindkey '^N' complete-menu
# TODO:
# - first way:
#   - <TAB> : next
#   - <SHIFT>+<TAB> : previous
#   - <SPACE> : ok
#   - <CTRL>+C : back
# - second way with <CTRL>+N (instead of <TAB>) : menu
#   - in menu:
#     - arrow keys
#     - same keys as in the first way (<TAB>, <SHIFT>+<TAB>...)
#     - <SPACE> : ok too
#     - <RET> : ok AND stay in the menu (very cool as a file finder; )
#     - <CTRL>+P : undo (eg. you want the word but you mistyped <RET> so <CTRL>+P & <SPACE> is the way)
bindkey -M menuselect '^N' accept-and-infer-next-history
bindkey -M menuselect '^P' undo
bindkey -M menuselect '^M' accept-and-hold
bindkey -M menuselect ' ' .accept-line
bindkey -M menuselect '*' history-incremental-pattern-search-forward
# alternative keys
bindkey -M menuselect '/' accept-and-infer-next-history
bindkey -M menuselect '^?' undo
# <SHIFT>+<TAB> reverses completion AND menu
bindkey '^[[Z' reverse-menu-complete
# vi motion keys
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
#
# experimenting with the old burke/matcher (requires matcher binary installed)
# ^X^N to use matcher in or off a git project
#
# set `activate_simple_matcher` to enable the simple version (^X^T; menu version only by default)
if (( $+commands[matcher] )); then
 #
 # SIMPLE VERSION
  _matcher_complete() {
    local expl
    _description -V files expl 'matching files'
    integer i=1
    (git ls-files 2>/dev/null || find .) | matcher --limit 30 ${words[CURRENT]} | while read line; do
      compadd "$expl[@]" -U -2 -V $i -- "$line"
      i=$((i+1))
    done
    compstate[insert]=menu
  }
  zle -C matcher-complete complete-word _generic
  zstyle ':completion:matcher-complete:*' completer _matcher_complete
  # (optional)
  # zstyle ':completion:matcher-complete:*' format '%F{red}-- <from burke/matcher> --%f'
  [[ -v activate_simple_matcher ]] && bindkey '^X^T' matcher-complete # (only use <CTRL-X><CTRL-N> to access the menu version)
  #
  # MENU VERSION (forced b/c of the NO_ALWAYS_LAST_PROMPT)
  zle -C matcher-menu menu-select _generic
  zstyle ':completion:matcher-complete-menu:*' completer _matcher_complete
  _matcher_menu()
  {
    setopt localoptions alwayslastprompt
    zle matcher-menu
  }
  zle -N matcher-complete-menu _matcher_menu
  bindkey '^X^N' matcher-complete-menu
else
  _warn_missing_matcher () { echo; print "install matcher to try this option\n"; zle redisplay }
  zle -N _warn_missing_matcher
  [[ -v activate_simple_matcher ]] && bindkey '^X^T' _warn_missing_matcher
  bindkey '^X^N' _warn_missing_matcher
fi
