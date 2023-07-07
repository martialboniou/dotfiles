(){
  local PHP_LOCALPATH="/opt/php@7.4"
  local PHP_FULLPATH="${HOMEBREW_PREFIX}${PHP_LOCALPATH}"
  [[ -d "${PHP_FULLPATH}" ]] &&
    export PATH="${PHP_FULLPATH}/bin:${PHP_FULLPATH}/sbin:$PATH" &&
    export LDFLAGS="-L${PHP_FULLPATH}/lib" &&
    export CPPFLAGS="-I${PHP_FULLPATH}/include"
}

