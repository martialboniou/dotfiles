;; everything about brackets (surround, paredit-like edition...)
(import-macros {: or=} :hibiscus.core)
[; auto-pairing (I'd like a magic tool that closes all the treesitter brackets
 ; when we want instead of a stupid pairing tool)
 {1 :windwp/nvim-autopairs
  ; TODO: disable? check in 4 weeks
  :event :InsertEnter
  :opts {:disable_filetype [:TelescopePrompt :minifiles :vim]
         ; echasnovski/mini.files
         :enable_check_bracket_line false}
  ; very annoying (does it work?)
  :config (fn [_ opts]
            (local pairs (require :nvim-autopairs))
            (local cond (require :nvim-autopairs.conds))
            (local rule (require :nvim-autopairs.rule))
            (pairs.setup opts)
            ;; lisp exceptions for quotes & backticks ; TODO: check this, deadline: 2 weeks
            (each [_ char (ipairs ["'" "`"])]
              (pairs.remove_rule char)
              (pairs.add_rule (: (: (rule char char) :with_pair
                                    (cond.not_before_regex_check "%w"))
                                 :with_pair
                                 (λ []
                                   (not (or= vim.bo.filetype :lisp :scheme
                                             :fennel :shen :clojure)))))))}
 ; classic quotes/brackets manipulation; eg: cs'" => change surroundings
 {1 :kylechui/nvim-surround :event :VeryLazy :config true}
 ; additional text objects; eg:
 ;   cin) => change inners of the parens (cursor out)
 ;   da,  => delete between commas (cursor in)
 ;   d2i) => change inners including outer parens (cursor in)
 {1 :wellle/targets.vim :event :VeryLazy}
 ; s-expression editing; eg:
 ;   >) => slurping forward
 ;   <( => barfing backward
 ; {1 :julienvincent/nvim-paredit
 ;  :event :BufReadPost
 ;  :opts {:extension {:fennel {:get_node_root :return}}}}
 ]
