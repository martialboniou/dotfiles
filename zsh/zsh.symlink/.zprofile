# .zprofile for macOS/openBSD/Linux
#
# Copyright (c) 2024-2025 <hondana@gmx.com>
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

# __PATH from `zshenv`
if [[ -n "${__PATH}" ]]; then
  # disable any /etc/zprofile reload
  export PATH="${__PATH}"
  # NOTE: unset in `zshrc`
fi

# ensure /etc/zprofile will be run once
# (useful on macOS where the Bourne-shell scripts located in `/etc/paths.d`
# will mess in an interactive login subshell)
if [[ -n "${__ZPROFILE_SOURCED:-}" ]]; then return; fi
export __ZPROFILE_SOURCED=1
