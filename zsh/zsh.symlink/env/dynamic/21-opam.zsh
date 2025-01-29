# on macOS, ensure homebrew binaries are in $path
() {
    if (( $+commands[opam] )); then
        eval $(opam env)
    fi
}
