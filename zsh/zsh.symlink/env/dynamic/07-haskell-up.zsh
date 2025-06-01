() {
  if [[ -x "/usr/local/bin/ghcup"
     || -x "/opt/homebrew/bin/ghcup"
     || -x "${HOME}/.ghcup/bin/ghcup"
     ]]; then
    local GHCUP_PATH="${HOME}/.ghcup"
    local GHCUP_BIN="${GHCUP_PATH}/bin"
    export PATH="${GHCUP_BIN}:$PATH"
  else # try stack-based installation
    local STACK_PATH="/usr/local"
    local STACK_BIN="${STACK_PATH}/bin"
    if [[ -r "${STACK_BIN}" ]]; then
      export PATH="${STACK_BIN}:$PATH"
    fi
  fi
  local STACK_LOCAL_PATH="${HOME}/.local"
  local STACK_LOCAL_BIN="${STACK_LOCAL_PATH}/bin"
  if [[ -r "${STACK_LOCAL_BIN}" ]]; then
    export PATH="${STACK_LOCAL_BIN}:$PATH"
  fi
}
