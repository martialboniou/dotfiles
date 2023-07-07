## this is the global composer; the composer.phar binary (renamed composer)
## should be in $HOME/.scrubs/bin as this directory is optional
() {
  local COMPOSER_LOCAL_PATH="${HOME}/.composer/vendor"
  local COMPOSER_LOCAL_BIN="${COMPOSER_LOCAL_PATH}/bin"
  if [[ -r "${COMPOSER_LOCAL_BIN}" ]]; then
    export PATH="${COMPOSER_LOCAL_BIN}:$PATH"
  fi
}

