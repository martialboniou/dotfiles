() {
  # ensure /usr/local is before nix/homebrew (you might not want that!)
  local LOCAL_BIN="/usr/local/bin"
  export PATH="${LOCAL_BIN}:$PATH"
}

