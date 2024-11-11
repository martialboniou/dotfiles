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

# OLD PAGER='less -X -M'
export PAGER='bat'
# if less, add 'source-highlight'
export LESSOPEN="| $SRC_HILITE_LESSPIPE %s"
export LESS=' -R '

# Manpath & Manualpage search order
export MANSECT=3:2:9:8:1:5:4:7:6:n
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANROFFOPT='-c'

# ~/.zsh/.zshrc will load your personal install of `fzf`
# here's the default settings from https://github.com/agilesteel/.dotfiles
export FZF_DEFAULT_COMMAND='fd --type f --color=never --hidden --exclude=.git'
export FZF_DEFAULT_OPTS='--no-height --color=bg+:#343d46,gutter:-1,pointer:#ff3c3c,info:#0dbc79,hl:#0dbc79,hl+:#23d18b'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview 'bat --color=always --line-range :50 {}'"
export FZF_ALT_C_COMMAND='fd --type d . --color=never --hidden --exclude=.git --exclude=.github'
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -50'"
