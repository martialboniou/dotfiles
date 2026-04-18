;; ensure python can import `neovim`:
;; - do `:checkhealth` here and search for `pynvim`
;; - install `pynvim`
;;   1. use `virtualenv` or `uv`
;;   2. alternatively, and on your own machine, install globally by typing:
;;     ```sh
;;     pip3 install pynvim --break-system-packages
;;     ```
{1 :kovisoft/slimv :ft :lisp}

;;; KEYBINDINGS:
;; - `,c` : connect
;; - `,e` : eval from a `lisp` filetype
;; - `,Q` : quit server

;;; TROUBLESHOOTING:
;; - try to launch a server manually: `sbcl -load ~/.local/share/nvim/lazy/slimv/slime/start-swank.lisp`
;; - RTFM if you need to boot another implementation (say, Franz Allegro CL):
;;   - https://github.com/kovisoft/slimv/blob/master/doc/slimv.txt
;; - ensure the version of python3 being able to import `neovim` from `pynvim`
;; is the same (check the previous line *ensure python can...*; I generally
;; don't want to manage a `.venv` activation each time I boot Neovim (and a
;; manually launched Lisp image) so I choose to install `pynvim` globally)
