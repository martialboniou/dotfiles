# .zshrc for macOS/debian
# config,alias,dvorakize 2010 by Martial <hondana@gmx.com>
# idea (c) 2001 by Robert Manea <rob dot manea at gmail dot com>
#
# Copyright (c) 2005-2013 <hondana@gmx.com>
# Copyright (c) 2021 <martial@gmx.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE
#
# required packages: source-highlight
# note: - dark background is the standard here
#       - dirstack is saved between sessions (only if there's directory
#       changes during the session); use the alias 'd' to get the dirs
#       listing

# files
ZDOT_SRC_ZSH=${ZDOTDIR}/zshrc.d
ZDOT_FUNCTIONS=${ZDOT_SRC_ZSH}/functions

# additional functions
if [[ -d $ZDOT_FUNCTIONS ]]; then
 fpath=($ZDOT_FUNCTIONS $fpath)
fi
export fpath
typeset -U fpath

# zshrc.d = debian-like zshrc sourcing
if [[ -d "${ZDOT_SRC_ZSH}" ]]; then
 foreach file in $(command ls -d ${ZDOT_SRC_ZSH}/* | grep -v $ZDOT_FUNCTIONS)
 source $file
 end
fi

# FZF configuration
[ -f $HOME/.fzf.zsh ] && source $HOME/.fzf.zsh
if (( $+commands[nvim] )); then
  alias "vim"="nvim"
fi

# opam configuration
[[ ! -r $HOME/.opam/opam-init/init.zsh ]] || source $HOME/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
