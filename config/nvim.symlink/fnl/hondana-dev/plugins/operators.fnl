;; various operators for words/brackets (surround, vim-exchange, paredit)
(import-macros {: or=} :hibiscus.core)

;; main loading event
(local event [:BufReadPost :BufNewFile])

(λ lisp-ft? [ft]
  (or= ft :lisp :scheme :fennel :shen :clojure))

(λ lisp-rules [tag]
  (local Rule (require :nvim-autopairs.rule))
  (local cond (require :nvim-autopairs.conds))
  (-> tag
      (Rule tag) ; same open/close tag
      (: :with_pair (cond.not_before_regex_check "%w"))
      (: :with_pair #(not (lisp-ft? vim.bo.filetype)))))

(local _autopairs
       {1 :windwp/nvim-autopairs
        : event
        ;; inactive in echasnovski/mini.files + lisp (paredit instead)
        :opts {:disable_filetype [:TelescopePrompt
                                  :minifiles
                                  :vim
                                  ;; disabled in lisp (if enabled, check :config)
                                  :lisp
                                  :scheme
                                  :fennel
                                  :shen
                                  :clojure]
               ;; very annoying (does it work?)
               :enable_check_bracket_line false}
        :config (λ [_ opts]
                  (local pairs (require :nvim-autopairs))
                  (pairs.setup opts)
                  ;; lisp exceptions for quotes & backticks
                  (each [_ char (ipairs ["'" "`"])]
                    (pairs.remove_rule char)
                    (pairs.add_rule (lisp-rules char))))})

(icollect [_ pkg (ipairs [:tommcdo/vim-exchange
                          [:kylechui/nvim-surround
                           #((. (require :nvim-surround) :setup) $2)]
                          [:kovisoft/paredit
                           ;; fancy keybindings
                           ;; <> : move left (like <leader><)
                           ;; >< : move right (like <leader>>)
                           #(each [direction keys (pairs {:Left "<>"
                                                          :Right "><"})]
                              (vim.keymap.set :n keys
                                              (-> direction
                                                  (#["<cmd>:call PareditMove"
                                                     $
                                                     "()<CR>"])
                                                  (table.concat))))]
                          [:opdavies/toggle-checkbox.nvim
                           #(vim.keymap.set :n :<leader>tt
                                            ":lua require('toggle-checkbox').toggle()<CR>")]])]
  (let [seq? (-> pkg (type) (= :table))
        url (if seq? (. pkg 1) pkg)
        spec {1 url : event}]
    (when seq?
      (tset spec :config (. pkg 2)))
    spec))

;;; DOC

;; VIM-EXCHANGE
;; swap two words easily
;; examples
;    cxiw => first time on the word A, prepare A for swapping
;            second time on a second word B, swap A and B

;; AUTOPAIRS (disabled)
;  CHECK: I'd like a magic tool that closes all the treesitter brackets
;         when we want instead of a stupid pairing tool

;; NVIM-SURROUND
;; classic quotes/brackets manipulation; eg: cs'" => change surroundings
;; additional text objects; eg: ; REM ((
;    cin) => change inners of the parens (cursor out)
;    da,  => delete between commas (cursor in)
;    d2i) => change inners including outer parens (cursor in)

;; PAREDIT
;; s-expression editing = paredit as recommended by monkoose in the nvlime-tutor
;; (https://github.com/monkoose/nvlime#Quickstart)
;
;  **normal mode** (most commands require to press the shift key)
;  <leader>( : toggle paredit (for single entrance) ; )
;  (/)       : previous opening/next closing parens until the top-level
;  [[/]]     : start of the previous/next defun/top-level sexp
;  <leader>>/<leader><       : move parens (NOTE: let the shift key down)
;  (local keybindings) ></<> : move parens (same as previous line)
;  <leader>J : join (a)|(b) -> (a b)
;  <leader>O : split (open; the opposite of <leader>J)
;  <leader>W : 
;  <leader>w( :   ;)
;  <leader>S : (BEWARE: don't forget the shift key; <leader>s = search/replace) 
;  <leader>Up/<leader>Down : 
;  <leader>I : 
;  
;  **visual mode**
;  (/)   : 
;  <leader>W : 
;  <leader>w( : ;)
;
;  IDEA of mapping (don't do this)
;  map <leader>l  <leader>>
;  map <leader>h  <leader><
;  map <localLeader>w  <leader>w(
;  map <localLeader>{  <leader>w{
;  map <localLeader>"  <leader>w"
;  map <localLeader>[  <leader>w[
;  map <leader>s  <leader>S ;; IMPORTANT: nope
;  map <leader>o  <leader>O
;  map <leader><leader>h  <leader><Up>
;  map <leader><leader>l  <leader><Down>
;  let g:paredit_smartjump=1
;
;; ready on first file if you need to enable for a non-lisp content
;  more info: https://github.com/kovisoft/slimv/blob/master/doc/paredit.txt
; use: `:cal PareditInitBuffer()`
;;; TODO: replace paredit by parinfer-rust if smoother
;;; NEXT = UNUSED
;; s-expression editing; eg:
;    <( => barfing backward
;    >) => slurping forward
;  {1 :julienvincent/nvim-paredit
;   :event :BufReadPost
;   :opts {:extension {:fennel {:get_node_root :return}}}}

;; NVIM-SURROUND
;; classic quotes/brackets manipulation; eg: cs'" => change surroundings
;
; <leader>tt : toggle checkbox (useful in markdown/org); append [ ] if nothing
;;; PROS: lightweight, no need mkdnflow or any specific tool
;;; CONS: no multi-switch, no way to customize the checked_character `x`

;;; DANGER! ;; targets ruin the Vim macros (recorded/typed)
;;; DANGER! ;; {1 :wellle/targets.vim}
