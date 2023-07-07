#!/usr/bin/zsh
#
# use ^U and ^P to go one level up in history and 
# go backwards respectively so we have no more 'cd ..'
# lines in our terminal

go-up () {
 cd ..
 zle reset-prompt
}; zle -N go-up

go-to-previous () {
 popd
 zle reset-prompt
}; zle -N go-to-previous

# prefer Ctrl (in ASCII standard VT way) than Alt
# note: ^U erase line but there's other bindings
#       to do so like <ESC>dd when `bindkeys -v`
# TODO: test vi or emacs modes
bindkey -M viins '^u' go-up
bindkey -M vicmd '^u' go-up
bindkey -M viins '^p' go-to-previous
bindkey -M vicmd '^p' go-to-previous
