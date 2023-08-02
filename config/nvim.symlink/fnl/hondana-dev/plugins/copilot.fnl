{1 :zbirenbaum/copilot.lua
 :cmd [:Copilot]
 :keys [{1 "<M-]>" :mode :i}]
 ; the next suggestion key will load me
 :opts (fn []
         (vim.api.nvim_create_user_command :CopilotTrigger
                                           (fn []
                                             ((. (require :copilot.suggestion)
                                                 :toggle_auto_trigger)))
                                           {}) ; NOTE: if cmp disturbs you, use <C-e>/<C-Space> to switch off/back
         {:suggestion {:keymap {; <M-=> was <M-l> but used by yabai
                                ; (memo: = to sync as in mini.files)
                                :accept :<M-=>
                                ; may be useful!
                                :accept_word "<C-]>"
                                :dismiss "<C-[>"
                                :next "<M-]>"
                                :prev "<M-[>"}}})}
