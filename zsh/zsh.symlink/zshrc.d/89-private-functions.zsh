#!/usr/bin/env zsh
# my private functions
() {
  local private_bundle=scrubs # choose a folder wisely
  local private_functions="$HOME/.$private_bundle/share/zsh/functions"
  [[ -r "$private_functions" ]] && fpath=( "$private_functions" $fpath ) && autoload _$private_bundle # load the launcher named _<bundle>
}
