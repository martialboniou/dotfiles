(import-macros {: funcall! : tc} :hondana-dev.macros)

(macro set-value-keys! [val ...]
  `(collect [# s# (ipairs [,...])]
     s#
     ,val))

(tc type "table<string, true>")
(local prompts (set-value-keys! true :java :lua :php :ts))

(tc param post_leader_key string)
(tc param refactor_type string)
(tc param ?mode "string?|string[]?")
(tc return
    "{[1]: string, [2]: fun(): nil, mode: string|string[], desc: string}")

(λ bind [post-leader-key refactor-type ?mode]
  {1 (.. :<leader> post-leader-key)
   2 #(funcall! :refactoring :refactor refactor-type)
   :mode (or ?mode :n)
   :desc (.. "Refactor: " (string.lower refactor-type))})

(tc type LazySpec)
(local P ;;
       {1 :ThePrimeagen/refactoring.nvim
        :cmd [:Refactor]
        :opts {:prompt_func_return_type prompts
               :prompt_func_param_type prompts
               :printf_statements {}
               :print_var_statements {}}
        :dependencies [:nvim-lua/plenary.nvim
                       :nvim-treesitter/nvim-treesitter
                       {1 :nvim-telescope/telescope.nvim
                        :config #(let [{: load_extension} (require :telescope)]
                                   (load_extension :refactoring))}]
        :keys [{1 "<leader>rt"
                2 #(let [{:extensions {:refactoring {: refactors}}} (require :telescope)]
                     (refactors))
                :mode [:n :x]
                :desc "Select a refactor (with Telescope)"}
               (unpack [(->> [;; visual only
                              [:re "Extract Function" :x]
                              [:rf "Extract Function To File" :x]
                              [:rv "Extract Variable" :x]
                              ;; both mode
                              [:ri "Inline Variable" [:x :n]]
                              ;; normal
                              [:rb "Extract Block"]
                              (->> [:rbb :rbf] ;; rbb = ERGO version of rbf
                                   (vim.tbl_map #[$ "Extract Block To File"])
                                   (unpack))]
                             (vim.tbl_map #(bind (unpack $)))
                             (unpack))])]})

P
