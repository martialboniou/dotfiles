export HOMEBREW_NO_ENV_HINTS=
# export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
() {
  eval $(/opt/homebrew/bin/brew shellenv)
  if [[ -r "$(brew --prefix llvm)" ]]; then
    export PATH="$(brew --prefix llvm):$PATH"
    # append a specific version to the $path:
    #   `export PATH="$(brew --prefix llvm@18):$PATH"`
    # ensure llvm is linked (it should already be linked):
    #   `brew link llvm --force`
    # don't do the following:
    #   `cd /opt/homebrew/opt && rm llvm && ln -s ../Cellar/llvm@18/18.1.8 llvm`
  fi
}
