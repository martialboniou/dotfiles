# .zshrc for macOS/openBSD/Linux
# config,alias,dvorakize 2010 by Martial <hondana@gmx.com>
# idea (c) 2001 by Robert Manea <rob dot manea at gmail dot com>
#
# Copyright (c) 2005-2013 <hondana@gmx.com>
# Copyright (c) 2021,2025 <martial@gmx.com>
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
#       - source ONCE the path here instead of using `.zshenv`
#         - since end of june 2025
#       - the local fpath is ${HOME}/.zsh/zshrc.d/functions; you will find:
#         - additional completions, by convention, starting with a `_`
#         (eg. `_rustup`)
#         - other functions starting with no `_` (except _guidirs)

## -- this code can be changed between subshells --

# disable conda (set to 1 otherwise)
export __CONDA_ENABLED=0

# your developer project home
DOCUMENTS="${HOME}/Documents"
export DEVELOPER_ROOT="${DOCUMENTS}/Code"
hash -d dev="${DEVELOPER_ROOT}"
hash -d zig="${DEVELOPER_ROOT}/zig"
hash -d dot="${DEVELOPER_ROOT}/dotfiles"
hash -d vim="${HOME}/.config/nvim"

# misc. hashes
hash -d doc="${DOCUMENTS}/Notebook"

# files
zdot_sources_path=${ZDOTDIR}/zshrc.d
export ZDOT_PROMPT=${ZDOTDIR}/prompts # see $ZDOTDIR/zshrc.d/06-prompt.zsh

# additional functions
() {
  local zdot_fpath=${zdot_sources_path}/functions
  () {
    for dir in "$@"; do
      dir=${dir:A}
      [[ ! -d "$dir" ]] && return
      fpath=("$dir" $fpath[@])
    done
  } $zdot_fpath $zdot_fpath/custom $zdot_fpath/dev
}
export fpath
typeset -U fpath

# user path
if [[ -z "${__ZSHRC_SOURCED:-}" ]]; then
  ## -- this code won't be reloaded --
  
  # opam configuration
  [[ ! -r $HOME/.opam/opam-init/init.zsh ]] || \
    source $HOME/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

  # bun completions
  [ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

  # direnv hook
  if (( $+commands[direnv] )); then
      eval "$(direnv hook zsh)"
  fi

  # local path
  autoload prepath
  source $ZDOTDIR/.zshrc.local_path

  # THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
  export SDKMAN_DIR="$HOME/.sdkman"
  [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && \
    source "$HOME/.sdkman/bin/sdkman-init.sh"
fi
# IMPORTANT TAG: don't remove me
export __ZSHRC_SOURCED=1

# setup in $ZDOTDIR/zshrc.d = debian-like sourcing with numbers for precedence
# IMPORTANT: at this point, the PATH built from /etc/zprofile, /etc/zshrc &
# your additional paths from this file (which loads the files from
# $ZDOTDIR/paths) must be complete in order to have consistent settings
if [[ -d "${zdot_sources_path}" ]]; then
  foreach file in $(find $zdot_sources_path -name "[0-9]*.zsh" -type f | sort -n 2>/dev/null)
    # environment, alias, prompts...
    source $file
  end
fi
unset ZDOT_PROMPT # see $ZDOTDIR/zshrc.d/06-prompt.zsh

# FZF configuration 
# .fzf.zsh can override any variables set in
# `$ZDOTDIR/zshrc.d/20-environment.zsh`
[ -f $HOME/.fzf.zsh ] && \
  source $HOME/.fzf.zsh

unset __PATH
unset zdot_sources_path
# typeset -U -g PATH

if [[ "${__CONDA_ENABLED}" -gt 0 && "`uname`" == "Darwin" ]]; then
# WARN: check `~/.zsh/zshrc.d/06-prompt.zsh`
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
fi

# mise shims
if (( $+commands[mise] )); then
  eval "$(mise activate zsh)"
fi
