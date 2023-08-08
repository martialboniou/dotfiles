Emacs 29 setup
==============

Goal
----

- small setup (no time for crazy customization as NeoVim is my main editor in
  2023)
- Common Lisp ready
- Evil Mode activated
- (may include some tools from the deprecated `2013-scripts`; org-mode,
  wanderlust, mu4e...)

Context
-------

- `emacs -nw` on iTerm2 in tmux session
- SBCL 2.3.7 on macOS 13.4

Notes
-----

- `M-` (AKA `Alt`) works out of the box on Emacs 29 on macOS
  - GUI: ensure `(featurep 'ns)` or the `--with-ns` build option
    with `C-h v system-configuration-options`
  - term (what I use): iTerm2 must have `Esc+` enabled (beware of conflicts;
    I use skhd for yabai's keybindings & have disabled M-x/M-y in my
    current setup)

