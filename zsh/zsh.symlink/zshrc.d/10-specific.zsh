# ---[ Specific ]------------------------------------------------------

# MPD daemon
# `ls` colors & dircolors
# source-highlight
# OpenStep `open` command for all systems

# Mono script directory
MONO_SCRIPT_BIN=$HOME/.tools/scripts

# W3M powered markup language translator
# using ruby gems
alias html-w3m='w3m -T text/html' # check 50-alias.zsh
function org-w3m () {
  org-ruby -t html $1 | html-w3m
}
function markdown-w3m () {
  rdiscount $1 | html-w3m
}
alias -g md-w3m='markdown-w3m'
function textile-w3m () {
  redcloth $1 | html-w3m
}

# Mac OS X specificities first
if [ "`uname`" = "Darwin" ]; then
  # MPD daemon launch issue on Mac OS X 10.5
  #type detach &>/dev/null && alias mpd="detach mpd --no-daemon" || echo "MPD doesn't work as expected"
  SRC_HILITE_LESSPIPE=src-hilite-lesspipe.sh # port install source-highlight
  OPEN_COMMAND='open'
  export LSCOLORS=dxgxcxdxbxegedabagacad
  alias ls='ls -G' # AIX/BSD ls command
  if (( $+commands[gtar] )); then
    alias tar='gtar' # GNU tar = gtar
  fi
  function wman() {
    url="man:${1}"
    echo `open $url` # install Bwana; ManOpen is DEPRECATED
  }
  alias gitk="/usr/bin/wish $(which gitk)"
  MAN_COMMAND=man # or wman
else # Default
  test -r "$HOME/.dircolors" && eval `dircolors -b $HOME/.dircolors`
  SRC_HILITE_LESSPIPE=/usr/share/source-highlight/src-hilite-lesspipe.sh
  OPEN_COMMAND='gnome-open' # I prefer Gnome to XFCE but I switch metacity for Xmonad
  alias ls='ls --color=auto'
  alias -g open="$OPEN_COMMAND"
  if [[ -x `which ooimpress` ]]; then
    alias -s ppt='ooimpress &> /dev/null '
  fi
fi
# check 50-alias.zsh to avoid overrides
