() {
  local STACK_PATH="/usr/local"
  local STACK_LOCAL_PATH="${HOME}/.local"
  local STACK_BIN="${STACK_PATH}/bin"
  local STACK_LOCAL_BIN="${STACK_LOCAL_PATH}/bin"
  if [[ -r "${STACK_BIN}" ]]; then
    export PATH="${STACK_BIN}:$PATH"
  fi
  if [[ -r "${STACK_LOCAL_BIN}" ]]; then
    export PATH="${STACK_LOCAL_BIN}:$PATH"
  fi
}
