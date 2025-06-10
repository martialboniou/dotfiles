#!/usr/bin/env zsh

# Run me to install/update 

DEST="${HOME}/.zsh/zshrc.d/functions/custom"
echo "Install additional completions for zsh; it might take some time..."

() {
  # Zig
  cd "${DEST}"
  curl -LO "https://raw.githubusercontent.com/ziglang/shell-completions/master/_zig" > /dev/null 2>&1
  [[ "$(file -b --mime-type _zig)" = text/html ]] && rm _zig

  # Nix
  apps=( -build -channel -collect-garbage -common-options -copy-closurex-env \
    -hash -install-package -instantiate -prefech-url -push -shell -store )
  if [[ -r "/etc/NIXOS" ]]; then
    apps+=( os-build-vms os-container os-generate-config os-install os-option \
      os-rebuild os-version )
  fi
  for a in "${apps[@]}"; do
    eval "curl -LO \"https://raw.githubusercontent.com/nix-community/nix-zsh-completions/master/_nix${a}\" > /dev/null 2>&1"
    eval "[[ \"$(file -b --mime-type _nix${a})\" = text/html ]] && rm \"_nix${a}\""
  done

  # Rust conditionally
  if (( $+commands[rustup] )); then
    apps=( rustup cargo )
    for a in "${apps[@]}"; do
      eval "rustup completions zsh ${a} > _${a}"
    done
  fi

  echo "done"
}
