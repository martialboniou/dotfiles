# install rustup manually
# then from there:
# - rustc
# - cargo
CARGO_HOME="${HOME}/.cargo"
() {
  local CARGO_LOCAL_ENV="${CARGO_HOME}/env"
  if [[ -r "${CARGO_LOCAL_ENV}" ]]; then
    source $CARGO_LOCAL_ENV
  fi
}
