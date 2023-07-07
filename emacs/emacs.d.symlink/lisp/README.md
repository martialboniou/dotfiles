Add me to your `EMACSLOADPATH` to boot Emacs in partial configuration:

Examples
========

Boot a minimalist Emacs setup (including `vars`, `defs`, `walker`, `adapter`, `appearance`, `window-manager`, `shortcuts`):

    $ emacs -q -l kernel

Boot an Emacs for coding (including `kernel`, `formats`, programming languages configuration):

    $ emacs -q -l code

Boot an Emacs for Org-mode based GTD (including `kernel`, `gtd`):

    $ emacs -q -l gtd

Programming Languages
=====================

The subdirectory `lang` should be added to your `EMACSLOADPATH` too. It enables specific languages programming support. For example, to boot Emacs in for `python` exclusively:

    $ emacs -q -l python-357

This command loads Emacs with `code` without other programming languages support than `python` through the *python-357.el* setup script.

Load-path Generator
===================

This `zsh` script makes the `EMACSLOADPATH` by adding:

* *~/.emacs.d/lisp*
* *~/.emacs.d/lisp/lang*

in your path. Add it to your `.zshrc` or build a function to autoload:

    build_emacs_load_path(){
      if (( $+commands[emacs] )); then
        local elp=`emacs -batch \
                  -eval "(princ (mapconcat #'(lambda (e) (format \"%s\" e)) load-path \":\"))"`
        if [[ "/" = "`echo $elp | awk 'BEGIN{FS=\"\"}{print $1}'`" ]]; then
          # emacs 'LOAD-PATH looks like a path
          LISP_PATH=${HOME}/.emacs.d/lisp
          LANG_LISP_PATH=${LISP_PATH}/lang
          export EMACSLOADPATH="${LISP_PATH}:${LANG_LISP_PATH}:$elp"
        fi
      fi
    }

To fasten your `zsh` startup. Write this too:

    fetch_emacs_load_path(){
      local path_file="${ZDOTDIR}/.emacsloadpath"
      if [[ -a ${path_file} && "${+parameters[force_reload]}" -eq 0 ]]; then
        source ${path_file}
      else
        build_emacs_load_path
        echo "export EMACSLOADPATH=${EMACSLOADPATH}" > ${path_file}
      fi
    } && fetch_emacs_load_path

    # MEMO: rehash
    reloadpath () {
      local force_reload
      fetch_emacs_load_path
    }
