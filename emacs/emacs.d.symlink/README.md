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
  - term (what I use): iTerm2 must have `Esc+` enabled
  - BEWARE: `M-` keys may conflict with other applications
    - `tmux`: `M-a` has been used as a **prefix**; the current prefix
      is `F5` (user reserved on Emacs; alias with `fn a` on macOS if
      `skhd` is on); check:
      - `../../config/tmux.symlink/tmux.conf`
      - `../../config/skhd.symlink/skhdrc`
    - I use `skhd` for yabai's keybindings too; fortunately my keybindings
      previously based on `Option` (AKA `Alt`) are now based on `fn`

