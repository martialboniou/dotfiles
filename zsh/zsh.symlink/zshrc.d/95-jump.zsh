#!/usr/bin/env zsh
# $ gem install jump
# $ ruby -e "require 'rubygems';print Gem::Specification::find_by_name('jump')"
# $ cp `gem contents jump | grep zsh` ~/.jump_shell_driver

[[ -x `which jump-bin` ]] && [[ -a "$HOME/.jump_shell_driver" ]] && source "$HOME/.jump_shell_driver" && \
alias j=jump && \
alias ja='jump --add' && \
alias jj='jump --list'
