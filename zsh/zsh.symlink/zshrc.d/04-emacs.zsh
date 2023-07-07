# ---[ Emacs ]---------------------------------------------------------

emacs_acquaint(){
  [[ "$TERM" == "eterm-color" ]] && TERM=xterm-256color && return 0
  return 1
}

# generate EMACS load-path and append your configuration directory (here .emacs.d/lisp)
build_emacs_load_path(){
  if (( $+commands[emacs] )); then
    unset EMACSLOADPATH
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
