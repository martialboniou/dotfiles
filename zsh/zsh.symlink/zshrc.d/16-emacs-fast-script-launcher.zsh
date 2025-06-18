#!/usr/bin/env zsh
# Emacs fast script launcher
# - to start emacs with a loading script at startup
# - to evaluate the script content in your current emacs REPL from the shell
# - to get some quick shortcuts to launch emacsclient
#   - the previously available `#` alias to start a script in the context of
#   emacsclient has been removed
#

DEBUG=0
# IMPORTANT: emacs ought to have `(server-start $USERNAME)` at startup
EMACS=emacs
EMACSCLIENT=emacsclient
EMACSNAMESERVER=$USERNAME

if [[ -d "${EMACSEN_SPATH:=${HOME}/.emacs.d/scripts}" ]]; then
    foreach scpt in $(command ls -d ${EMACSEN_SPATH}/*.el | awk '{sub(/\.el$/,"",$1);print}')
    ALIAS_SCRIPT=`echo $scpt | awk '{sub(/.*[\/]/,"",$1);print}'`
    if [[ $DEBUG > 0 ]]; then
      echo "$scpt =>  #$ALIAS_SCRIPT"
    fi
    alias -- \#$ALIAS_SCRIPT="$EMACSCLIENT --socket-name=$EMACSNAMESERVER --eval \"(progn (raise-frame (selected-frame))`cat ${scpt}.el`)\" 2>/dev/null || $EMACS -l $scpt"
    end
fi
alias emacsclient="$EMACSCLIENT --socket-name=$EMACSNAMESERVER --alternate-editor=vemacs"
