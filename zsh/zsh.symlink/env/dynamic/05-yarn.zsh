## https://classic.yarnpkg.com/lang/en/docs/cli/global
() {
    if (( $+commands[yarn] )); then
        export PATH="$(yarn global bin):$PATH"
    fi
}
