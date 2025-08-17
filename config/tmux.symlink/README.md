TMUX
====

- About the prefix
  - `fn a` is the current prefix (if skhd is active on macOS)
  - `F5` otherwise (user reserved in emacs)
  - (macOS only) ensure iTerm2 has Profiles/Keys/Esc+ for Option keys
  - previous ones:
    - `<C-b>` used by vim (snippet backward)
    - `<C-a>` navigate cursor in terms
    - `<C-Space>` is used in my vim configuration (start completion); also
      used in emacs
    - `<C-q>` is used by emacs
    - `<M-a>` (was the previous choice) is used by emacs
- Keybindings
  - `F5 c` : create new window
  - `F5 a` : last window
  - `F5 :` : open the tmux prompt
  - `F5 o` : (via tpm) tmux-sessionx
  - `F5 "` : split the current pane vertically
  - `F5 ;` : switch between panes
  - `F5 &` : kill window
  - `F5 n` : next window (`F5 l` too)
  - `F5 p` : previous window (`F5 h` too)
  - `F5 j` : window-pane down
  - `F5 k` : window-pane up
  - `F5 q` : print the pane numbers (& their sizes)
