# ---[ ZSH Prompts ]----------------------------------------------------
[[ -n "${ZDOT_PROMPT}" ]] && prompt_path="${ZDOT_PROMPT}" || prompt_path="${ZDOTDIR}/prompts"
emacs_acquaint # minimal prompt for emacsen vt
[[ $? -eq 0 ]] && source $prompt_path/minimal || source $prompt_path/advanced

# special cases (python envs)
export VIRTUAL_ENV_DISABLE_PROMPT=1
if (( $+commands[conda] )); then
  conda config --set changeps1 False
fi

setprompt

unset ZDOT_PROMPT
