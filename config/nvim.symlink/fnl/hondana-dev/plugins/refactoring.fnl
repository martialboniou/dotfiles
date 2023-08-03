(macro set-value-keys! [val ...]
  `(let [keys# ,[...]]
     (collect [# s# (ipairs keys#)]
       s#
       ,val)))

(fn bind [post-leader-key refactor-type mode]
  {1 (.. :<leader> post-leader-key)
   2 #(#($.refactor refactor-type) (require :refactoring))
   :mode (or mode :n)
   :desc (.. "Refactor: " (string.lower refactor-type))})

(local prompts (set-value-keys! true :java :lua :php :ts))

{1 :ThePrimeagen/refactoring.nvim
 :cmd [:Refactor]
 :opts {:prompt_func_return_type prompts
        :prompt_func_param_type prompts
        :printf_statements {}
        :print_var_statements {}}
 :dependencies [:nvim-lua/plenary.nvim :nvim-treesitter/nvim-treesitter]
 :keys [(->> [;; visual only
              [:re "Extract Function" :x]
              [:rf "Extract Function To File" :x]
              [:rv "Extract Variable" :x]
              ;; both mode
              [:ri "Inline Variable" [:x :n]]
              ;; normal
              [:rb "Extract Block"]
              (->> [:rbb :rbf] ;; rbb = ERGO version of rbf
                   (vim.tbl_map #[$ "Extract Block To File"]) (unpack))]
             (vim.tbl_map #(bind (unpack $)))
             (unpack))]}
