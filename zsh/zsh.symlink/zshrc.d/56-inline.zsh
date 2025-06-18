# ---[ Inline functions ]----------------------------------------------
setenv() { typeset -x "${1}${1:+=}${(@)argv[2,$#]}" }  # csh compatibility
freload() { while (( $# )); do; unfunction $1; autoload -U $1; shift; done }
# Simple commandline calculator
function calc () {
    awk "BEGIN { print $@ }"
}
# Cat language
# function cat-lang () {
#   if [[ -f "${MONO_SCRIPT_BIN}/cat_mono.exe" ]]; then
#     mono ${MONO_SCRIPT_BIN}/cat_mono.exe
#   else
#     echo "you need to install Cat executable from http://www.cat-language.com to ${MONO_SCRIPT_BIN}"
#   fi
# }
