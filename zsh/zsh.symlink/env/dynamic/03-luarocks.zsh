() {
  local LUAROCKS_LOCAL_PATH="${HOME}/.luarocks"
  local LUAROCKS_LOCAL_BIN="${LUAROCKS_LOCAL_PATH}/bin"
  if [[ -r "${LUAROCKS_LOCAL_BIN}" ]]; then
    export PATH="${LUAROCKS_LOCAL_BIN}:$PATH"
  fi
}

