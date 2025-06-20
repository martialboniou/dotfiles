# (optional) Mason packages installed via NeoVim
() {
  local MASON_LOCAL_PATH="${HOME}/.local/share/nvim/mason"
  local MASON_LOCAL_BIN="${MASON_LOCAL_PATH}/bin"
  if [[ -r "${MASON_LOCAL_BIN}" ]]  then
    export PATH="${MASON_LOCAL_BIN}:$PATH"
  fi
}
