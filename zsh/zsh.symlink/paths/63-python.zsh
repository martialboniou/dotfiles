() {
  local UNVERSIONED_PY="${HOMEBREW_REPOSITORY}/opt/python/libexec/bin"
  if [[ -d "${UNVERSIONED_PY}" ]]; then
    export PATH="${UNVERSIONED_PY}:$PATH"
  fi
#  local MAJOR=$(echo -e "import sys\nprint sys.version_info.major" | python 2>/dev/null)
#  local MINOR=$(echo -e "import sys\nprint sys.version_info.minor" | python 2>/dev/null)
#  local SITE="/usr/local/lib/python${MAJOR}.${MINOR}/site-packages"
#  [[ "$MAJOR" != "" ]] && [[ "$MINOR" != "" ]] && [[ -d "${SITE}" ]] && PYTHONPATH="${SITE}" && typeset -U -g PYTHONPATH && export PYTHONPATH
}
