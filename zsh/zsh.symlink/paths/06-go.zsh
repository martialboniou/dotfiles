# 1/ check `go env`; if set and ok, skip the rest
# 2/ create `$HOME/.local/share/go`
# 3/ set `go env GOPATH=$HOME/.local/share/go`
() {
    if (( $+commands[go] )); then
        export GOPATH="${HOME}/.local/share/go"
        prepath "${GOPATH}/bin"
    fi
}
