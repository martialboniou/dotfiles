`#` to rule em(acs) all
=======================

Those scripts are specific emacs launcher. Say, you want to start wanderlust, type:

  emacs -l ~/.emacs.d/scripts/mail

This Zsh configuration supports functions to launch it automatically (set EMACSEN_SPATH in your ~/.zshenv to point to another path than the default path "~/.emacs.d/scripts"). Use:

  #mail

to get an emacsclient loading the 'mail.el' script content in your current emacs image or to start a new emacs loading 'mail.el[c]' at startup if no emacs server is running.

This script should be used with `zsh` *Martialism* configuration and especially the `zsh` script named `.zsh/zshrc.d/95-emacs-fast-script-launcher.zsh`.
