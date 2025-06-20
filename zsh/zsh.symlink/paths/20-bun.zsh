() {
  local BUN_LOCAL_PATH="${HOME}/.bun"
  local BUN_LOCAL_BIN="${BUN_LOCAL_PATH}/bin"
  if [[ -r "${BUN_LOCAL_BIN}" ]]; then
    export BUN_INSTALL="$BUN_LOCAL_PATH"
    export PATH="${BUN_LOCAL_BIN}:$PATH"
  fi
}

