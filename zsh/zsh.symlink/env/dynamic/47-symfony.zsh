() {
  local SYMFONY_LOCAL_PATH="${HOME}/.symfony"
  local SYMFONY_LOCAL_BIN="${SYMFONY_LOCAL_PATH}/bin"
  if [[ -r "${SYMFONY_LOCAL_BIN}" ]]; then
    export PATH="${SYMFONY_LOCAL_BIN}:$PATH"
  fi
}

