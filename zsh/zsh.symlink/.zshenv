# .zshenv for macOS/openBSD/Linux
#
# Copyright (c) 2005-2021,2024-2025 <hondana@gmx.com>
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

# NOTE: IMPORTANT: you MUST delete ~/.zsh/cache/zshenv if you want to regenerate
# your PATH and the other variables created dynamically the first time

add_path () {
  [[ -d "$1" ]] && path=("$1" $path)
}

absolute_path () {
  echo $(perl -e 'use Cwd "abs_path"; print abs_path("$1");')
}

# source one or each file in a path
_source_path () {
  if [[ -f "$1" && -r "$1" ]]; then
    source "$1"
    return 0
  elif [[ -d "$1" && -x "$1" ]]; then
    foreach file in $(find $1 -name "[0-9]*.zsh" -type f | sort -r 2>/dev/null)
      source $file
    end
    return 0
  else
    return 1
  fi
}

# flush path
_flush_path () {
  typeset -U -g PATH
}

# generate basic path from env/static
_build_path () {
  local SYSTEM=`uname`
  local ADMIN_ACTION=1           # 0 not to reach sbin subdirs
  local env_directory=${ZDOTDIR}/env/static
  _source_path $env_directory || unset path # reset when nothing
  # TODO: check msys path
  [[ -d "/usr/X11/bin" ]] && path=($path /usr/X11/bin)
  path=($path /usr/bin /usr/libexec /bin)
  [[ $ADMIN_ACTION -gt 0 ]] && [[ -d "/usr/sbin" ]] &&  path=($path /usr/sbin)
  [[ $ADMIN_ACTION -gt 0 ]] && [[ -d "/sbin" ]] && path=($path /sbin)
  #_flush_path
}

# complete global path from env/dynamic
_complete_path () {
  local env_directory=${ZDOTDIR}/env/dynamic
  _source_path "$env_directory" || return 0
}

# get the full path
_fetch_path () {
  if [[ -o login || -n "${__ETC_PROFILE_NIX_SOURCED:-}" ]]; then
    # - skip this function in order to keep the PATH when invoking `zsh -l` in an interactive
    # session AND when `__ETC_PROFILE_NIX_SOURCED` is set
    #   - manpath/luapath/path is LOCALLY set here in zshenv for interactive instances only
    #     - we use zshenv because we can calculate the static/dynamic path for once
    #     - the cache file named $ZDOTDIR/cache/zshenv MUST be removed if you add some tools
    #     (eg: `golang`)
    #   - path is set in zshrc too (opam, rust, nix...); this operation is slower but required for
    #   additional functions to run OR additional variables to be available
    #   - `nix-daemon.sh` is loaded once so DON'T change the PATHs in this case otherwise a
    #   subshell (started with `zsh`) wouldn't have neither `/nix/var/nix/profiles/default/bin`
    #   nor `${HOME}/.nix-profile/bin`
    # also 
    return
  fi
  # TODO: write a script to purge this file
  local cache_file="${ZDOTDIR}/cache/zshenv"
  if [[ -r ${cache_file} ]]; then
    source ${cache_file}
  else
    ## STATICALLY GENERATED ENV
    local path_file="${ZDOTDIR}/env/static/autoload_path"
    if [[ -r ${path_file} && "${+parameters[force_reload]}" -eq 0 ]]; then
      source ${path_file}
    else
      _build_path
      echo "PATH=${PATH}" > ${path_file}
    fi
    ## DYNAMICALLY GENERATED ENV (from scripts/binaries; eg: `luarocks path --bin`)
    # NOTE:
    # load static/dynamic and save it in __ENV_PATH; in .zprofile: append it
    # BEFORE the path generated by /etc/profile and unset __ENV_PATH
    # (macOS uses this setup to ensure its tools are available)
    _complete_path
    env_vars=( PATH LUA_PATH LUA_CPATH GOPATH HOMEBREW_NO_ENV_HINTS HOMEBREW_EDITOR )
      # NIX_PROFILES NIX_SSL_CERT_FILE )
    truncate -s 0 ${cache_file}
    for ev in "${env_vars[@]}"; do
      print export ${ev}=\"${(P)ev}\" >>| ${cache_file}
    done
    # NOTE: IMPORTANT variable to ensure this function is called ONCE too (see below)
  fi
  export __ENV_PATH=${PATH}
  # normally unused
  [[ -v MANPATH && ! ${MANPATH} == "" ]] && export __ENV_MANPATH=${MANPATH}
}

# NOTE: don't fetch path if already done
# (reattach-to-user-namespace in tmux.conf will launch .zshenv TWICE; don't redo)
[ -v __ENV_PATH ] || _fetch_path

# MEMO: rehash
# FIX: untested in this version (2024/11)
repath () {
  local force_reload
  if [[ "`uname -s`" == "Darwin" ]]; then
    # IMPORTANT: ensure reset before calling `path_helper -s'
    export PATH=/usr/bin
    if [[ -a "/etc/zshenv" ]]; then
      source /etc/zshenv
    fi
  else
    export PATH=/usr/bin:/bin # ensure reset
  fi
  _fetch_path
  source $ZDOTDIR/.zprofile
  rehash
}
