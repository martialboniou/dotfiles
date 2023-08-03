;; everything about brackets (surround, paredit-like edition...)
(import-macros {: or=} :hibiscus.core)

(λ lisp-ft? [ft]
  (or= ft :lisp :scheme :fennel :shen :clojure))

(λ lisp-rules [tag]
  (local Rule (require :nvim-autopairs.rule))
  (local cond (require :nvim-autopairs.conds))
  (-> tag
      (Rule tag) ; same open/close tag
      (: :with_pair (cond.not_before_regex_check "%w"))
      (: :with_pair #(not (lisp-ft? vim.bo.filetype)))))

;;; WIP
[; auto-pairing (I'd like a magic tool that closes all the treesitter brackets
 ; when we want instead of a stupid pairing tool)
 {;; TODO: disable? check in 4 weeks
  1 :windwp/nvim-autopairs
  :event :InsertEnter
  ;; inactive in echasnovski/mini.files
  :opts {:disable_filetype [:TelescopePrompt :minifiles :vim]
         ;; very annoying (does it work?)
         :enable_check_bracket_line false}
  :config (λ [_ opts]
            (local pairs (require :nvim-autopairs))
            (pairs.setup opts)
            ;; lisp exceptions for quotes & backticks ; TODO: check this, deadline: 3 weeks
            (each [_ char (ipairs ["'" "`"])]
              (pairs.remove_rule char)
              (pairs.add_rule (lisp-rules char))))}
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
