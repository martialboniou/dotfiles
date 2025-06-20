## this is the global composer; the composer.phar binary (renamed
## composer) should be in $HOME/.local/bin
() {
  local COMPOSER_LOCAL_PATH="${HOME}/.composer/vendor"
  local COMPOSER_LOCAL_BIN="${COMPOSER_LOCAL_PATH}/bin"
  if [[ -r "${COMPOSER_LOCAL_BIN}" ]]; then
    export PATH="${COMPOSER_LOCAL_BIN}:$PATH"
  fi
}

