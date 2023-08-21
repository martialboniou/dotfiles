Emacs 29.1 setup
================

Goal
----

- small setup (no time for crazy customization as NeoVim is my main editor in
  2023)
- Common Lisp ready (this setup will boot a REPL automatically)
- **Evil Mode** activated
  - *REMINDER*: `C-z` to go back to the `Normal` state from the `Emacs` one
  - `C-g` (normally *cancel command*) in `Insert` state switches back to
    `Normal` state (like `<C-c>` in this NeoVim setup; check
    `../../config/nvim.symlink/fnl/hondana-dev/remap.fnl`)
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
- `C-Enter` & `S-Enter` (required in Org-mode) are still mapped to a
  `[13;` derivatives (`C-M` AKA `Return`) or `C-J` on macOS due to the old TTY
  compatibility; on iTerm2, add two entries in the Profiles' *Key Mappings*:
  - `C-Enter` / Send Escape Sequence / `[13;5u`
  - `S-Enter` / Send Escape Sequence / `[13;2u`
  (check this
  [source](https://stackoverflow.com/questions/16359878/how-to-map-shift-enter))
