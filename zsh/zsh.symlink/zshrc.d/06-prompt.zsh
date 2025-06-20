# ---[ ZSH Prompts ]----------------------------------------------------
[[ -n "${ZDOT_PROMPT}" ]] && prompt_path="${ZDOT_PROMPT}" || prompt_path="${ZDOTDIR}/prompts"
emacs_acquaint # minimal prompt for emacsen vt
[[ $? -eq 0 ]] && source $prompt_path/minimal || source $prompt_path/advanced

setprompt


