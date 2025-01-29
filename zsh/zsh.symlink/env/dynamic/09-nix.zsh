() {
  file="$HOME/.nix-profile/etc/profile.d/nix.sh"
  if [[ -f "$file" ]]; then
    source "$file"
  fi
}
