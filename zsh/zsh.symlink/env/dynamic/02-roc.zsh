() {
  local ROC_LOCAL_PATH="${HOME}/Documents/Code/roc"
  local ROC_LOCAL_BIN="${ROC_LOCAL_PATH}/current/roc"
  if [[ -x "${ROC_LOCAL_BIN}" ]]; then
    export PATH="${ROC_LOCAL_BIN:h}:$PATH"
  fi
}
