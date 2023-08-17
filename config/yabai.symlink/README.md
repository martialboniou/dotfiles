YABAI
=====

- NOTE: `<app> --(re)start-service` to (re)start a service
  - `<app>` is yabai
  - `<app>` is skhd
- macOS recommended System Settings:
  - Desktop & Dock
    - `Automatically rearrange Spaces based on most recent use` set to false
    - `Automatically hide and show the Dock` set to true
- TIP:
    - use `ctrl + F2` to move focus to the menu bar
- skhd (`shift` not explicitly written but a capital letter)
  - IMPORTANT!: don't map `fn + a` as it's the current prefix for tmux
    (Technically, it's `F5` but skhd does the mapping!)
  - most used
    - `fn + h,j,k,l` to navigate
    - `fn + m` to maximize/restore
    - `fn + H,J,K,L` to swap windows (+ `shift`)
    - `fn + 1..` to move window to a specific space
    - **(NOT WORKING NOW!)** `ctrl + fn + 1..` to go to a specific space
      (if no window to focus, go to the space anyway!; also, REMINDER:
      `ctrl + left/right` works too!); **IMPORTANT**: use `ctrl + fn`, not
      `ctrl` only, when dealing with numbers (`ctrl + 6` can be reserved on
      (Neo)Vim as an alternative to `<C-^>`; useful if Vim cannot listen
      to this last one because of a layout with a *dead key* for the
      *caret*)
    - `ctrl  + fn + q/s` to quit/start yabai
  - others
    - `fn + r` to rotate clockwise
    - `fn + t` to toggle window float
    - `fn + e` to balance
    - `fn + p,n` to move window to prev/next space
    - `fn + x,y` to flip x,y axis
    - `ctrl + fn + h,j,k,l` to move & split
    - `fn + o,u` to change focus between external display (west,east)
- for faster Dock show/hide, `killall Dock` after:
  - `defaults write com.apple.dock autohide-delay -int 0`
  - `defaults write com.apple.dock autohide-time-modifier -float 0.4`
- also watch: https://www.youtube.com/watch?v=bCdcuJZux_g&ab_channel=Livakivi
