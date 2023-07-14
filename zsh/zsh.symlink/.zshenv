# .zshenv for macOS/ubuntu/debian
#
# Copyright (c) 2005-2021 <hondana@gmx.com>
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

# load /etc/paths.d
_load_path_helper () {
  if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
  fi
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
  _flush_path
}

# complete global path from env/dynamic
_complete_path () {
  local env_directory=${ZDOTDIR}/env/dynamic
  _source_path "$env_directory" || return 0
  _flush_path
}

# get the full path
_fetch_path () {
  local path_file="${ZDOTDIR}/env/static/autoload_path"
  if [[ -a ${path_file} && "${+parameters[force_reload]}" -eq 0 ]]; then
    source ${path_file}
  else
    _build_path
    echo "PATH=${PATH}" > ${path_file}
  fi
  _load_path_helper
  _complete_path
} && _fetch_path

# MEMO: rehash
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
  rehash
}
. "$HOME/.cargo/env"
