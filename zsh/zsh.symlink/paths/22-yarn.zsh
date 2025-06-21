## https://classic.yarnpkg.com/lang/en/docs/cli/global
() {
  (( $+commands[yarn] )) && prepath "$(yarn global bin)"
}
