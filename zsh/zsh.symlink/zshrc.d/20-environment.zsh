# ---[ Environment ]---------------------------------------------------
EMACSNAMESERVER=$USERNAME
() {
    local -a editors
    local editor
    # vemacs = emacs w/ vimpulse (Vim emu)
    editors=(
        "nvim"
        "vim"
        "emacsclient --socket-name=$EMACSNAMESERVER --alternate-editor=vim"
        "emacs -nw"
        "vi"
        "nano"
    )
    for editor in ${editors[@]}; do
        (( $+commands[$editor[(w)1]] )) && {
            export EDITOR="$editor"
            export VISUAL="$editor"
            export HOMEBREW_EDITOR="$editor"
            break
        }
    done
}
# Unicode Locale
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
#export LANG=fr_FR.UTF-8
#export LC_ALL=fr_FR.UTF-8
export PS_PERSONALITY='linux'

# Manpath & Manualpage search order
export MANSECT=3:2:9:8:1:5:4:7:6:n

# Syntax highlight for less with 'source-highlight'
PAGER='bat'
# PAGER='less -X -M'
export LESSOPEN="| $SRC_HILITE_LESSPIPE %s"
export LESS=' -R '
