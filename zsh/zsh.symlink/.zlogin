# .zlogin for macOS/openBSD/Linux
#
# Copyright (c) 2023-2025 <hondana@gmx.com>
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

# don't start tmux in emacsen vt
[[ "$TERM" == "eterm-color" ]] && exit 0

# if no session is started, start a new session
# http://chm.duquesne.free.fr/blog
if (( $+commands[tmux] )); then
  if test -z ${TMUX}; then
    tmux $* # $* to pass args like `chdir` at startup
  fi
  # when quitting tmux, try to attach if DTMUX is not unset
  while [[ -z ${TMUX} ]]; do
    nop() { }
    trap nop INT
    ( sleep 5; kill -INT $$ 2> /dev/null ) & disown %%
    echo -n "Don't re-attach last session [y/K/_]? "
    read -k1 DTMUX
    case ${DTMUX:-out} in
      y) break
        ;;
      K) while true; do; exit; done
        ;;
      out) echo "$(tput cuu1)$(tput dl1)*** timeout ***"; break
        ;;
      *) tmux attach || break
        ;;
    esac
  done
fi
