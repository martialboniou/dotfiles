;;; AUTOPAIRS
;; maintenance mode - currently testing with zig
(local {:nvim_create_user_command uc} vim.api)

(λ toggle-plugin []
  (let [core (require :ultimate-autopair.core)
        prefix "Autopairing"]
    (set core.disable (not core.disable))
    (-> core.disable (#(if $ :disabled :enabled)) (#[prefix $])
        (table.concat " ") (.. :...) (vim.notify vim.log.levels.INFO))))

{1 :altermo/ultimate-autopair.nvim
 :event [:InsertEnter :CmdlineEnter]
 ;; INFO: disable when macro-ing (why I don't enjoy autopairs or I prefer
 ;; snippets!)
 :init #(uc :ToggleAutopair toggle-plugin {})
 :branch "v0.6"
 :opts {;; not in command line
        :cmap false
        :extensions {;; not in lisp as kovisoft/paredit is always ON
                     ;; FIX: next line disabled b/c it makes the plugin slow
                     ;; :cond {:cond #(not ($.in_lisp))}
                     ;;
                     ;; not in telescope prompts
                     :filetype {:nft [:TelescopePrompt]}}}}
