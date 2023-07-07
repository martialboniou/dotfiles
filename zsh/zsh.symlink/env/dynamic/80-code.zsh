() {
  local VSCODE_PATH="/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
  if [[ -r "${VSCODE_PATH}" ]]; then
    export PATH="${VSCODE_PATH}:$PATH"
  fi
}

