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

# changes on 2025-06-20:
# - the PATH is set in .zshrc ONCE
# - if there's a /etc/profile file reloading some PATHS it will be cancelled
# in .zprofile using the temporary __PATH as a memory of the previous PATH
#   - motivation: subshell with login option won't touch to the PATH again
#     - useful for `reattach-to-user-namespace` on macOS to enable the pbcopy
#     inside a tmux session
#     - useful for having fast-loading subshells with consistent path even
#     with a `--login` option
#     - no overhead for any scripts (as only .zshenv is loaded)
#     - faster script and interactive instance loading
#   - bad point: need to open a terminal on PATH changes in order to have
#   the top level shell with the new PATH

if [[ -o interactive && -o login && -n "${__ZPROFILE_SOURCED}" ]]; then
  # login interactive case only, so when .zshrc is at least loaded
  export __PATH="${PATH}"
fi
