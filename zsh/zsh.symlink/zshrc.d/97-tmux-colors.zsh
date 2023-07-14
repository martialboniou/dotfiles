#!/usr/bin/env zsh
# TMUX COLORS
# set CONTAINING_TERM in tmux/screen to avoid race condition on TERM setting

case "$CONTAINING_TERM" in
  *256color)
    TERM=screen-256color
    unset CONTAINING_TERM
    ;;
esac
