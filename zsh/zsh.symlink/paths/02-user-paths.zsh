# user paths
() {
  local dirs=(scrubs tools local)
  for (( idx=${#dirs[@]}; idx>0; idx-- )); do
    export PATH="${HOME}/.${dirs[idx]}/bin:${PATH}"
  done
}

