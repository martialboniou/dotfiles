() {
  local ROC_LOCAL_BIN="${HOME}/Documents/Code/roc/current/roc"
  if [[ -x "${ROC_LOCAL_BIN}" ]]; then
    export PATH="${ROC_LOCAL_BIN:h}:$PATH"
  fi
}
