#!/bin/zsh
defaults write org.gnu.Emacs ToolBar -string no > /dev/null 2>&1 \
&& printf "\r\033[2K  [ \033[00;32mOK\033[0m ] User defaults changed for Emacs: no toolbar\n" \
|| printf "\r\033[2K  [\033[0;31mFAIL\033[0m] Cannot set the Emacs preferences: toolbar unchanged\n"

defaults write org.gnu.Emacs ScrollBar -string no > /dev/null 2>&1 \
&& printf "\r\033[2K  [ \033[00;32mOK\033[0m ] User defaults changed for Emacs: no scrollbar\n" \
|| printf "\r\033[2K  [\033[0;31mFAIL\033[0m] Cannot set the Emacs preferences: scrollbar unchanged\n"

