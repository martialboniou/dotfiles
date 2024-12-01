# OBSOLETE
DEFAULT_RUBY_VERSION=2.6.10

install_ruby_source () {
  [[ -s "$1" ]] && source "$1" && return 0
  return 1
}

_chruby_post_install () {
  local version
  local file
  file="$HOME/.ruby-version"
  if [[ -f "$file" ]]; then
    version=$(cat "$file")
  else
    version=$DEFAULT_RUBY_VERSION
  fi
  [[ "$(chruby | grep $version 2>/dev/null)" == "" && "$version" != "$DEFAULT_RUBY_VERSION" ]] && chruby $DEFAULT_RUBY_VERSION || chruby $version
}

() {
  local -a ruby_installer ruby_associate_post_install
  ruby_installs=( "/usr/local/share/chruby/chruby.sh" "$HOME/.rvm/scripts/rvm" )
  ruby_post_installs=( _chruby_post_install true )
  for (( i = 1; i <= ${#ruby_installs[@]}; i++ )); do
    if install_ruby_source "${ruby_installs[$i]}"; then
      local post_install=${ruby_post_installs[$i]}
      eval "$post_install" 2>/dev/null
      return
    fi
  done
}

unfunction install_ruby_source

# TEMPORARY
export PATH="/opt/homebrew/opt/ruby@3.2/bin:$PATH"
