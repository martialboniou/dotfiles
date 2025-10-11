# ---[ ZSH Options ]----------------------------------------------------
# General
unsetopt CORRECT_ALL

setopt   ALWAYS_TO_END BASH_AUTO_LIST NO_BEEP CLOBBER
setopt   AUTO_CD CD_ABLE_VARS MULTIOS CORRECT

# Job Control
unsetopt BG_NICE
setopt   CHECK_JOBS NO_HUP

# History
setopt   INC_APPEND_HISTORY EXTENDED_HISTORY HIST_IGNORE_ALL_DUPS
setopt	 EXTENDED_HISTORY HIST_EXPIRE_DUPS_FIRST
setopt   HIST_REDUCE_BLANKS HIST_SAVE_NO_DUPS HIST_IGNORE_DUPS
setopt   SHAREHISTORY

# Stay compatible to sh and IFS
setopt	 SH_WORD_SPLIT

# unsetopt AUTOPARAMSLASH # FIXME: reenable later
setopt   NOTIFY GLOBDOTS PUSHDTOHOME
setopt   RECEXACT LONGLISTJOBS
setopt   AUTORESUME PUSHDSILENT
setopt   AUTOPUSHD PUSHDMINUS RCQUOTES MAILWARNING

# `extended_glob` removed for Nix Flakes (if you reenable it, you will need to
# surround the `#`-expression (say, `.#hello`) with quotes)
# setopt   EXTENDEDGLOB

# Don't expand files matching:
fignore=(.o .c~ .old .pro)
