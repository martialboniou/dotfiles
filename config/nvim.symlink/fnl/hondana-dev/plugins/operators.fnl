;; various operators for words/brackets (surround, vim-exchange, paredit, matchup, ...)
(import-macros {: set!} :hibiscus.vim)
(import-macros {: tc} :hondana-dev.macros)

;; paredit choice (NOTE: may proc autopairs)
(tc type string|boolean)
(local paredit-version (not :julienvincent))

;; list of filetypes where `gt` toggles a checkbox (eg. in a list)
(tc type "string[]")
(local checkbox-text-fts [:markdown :org :text :asciidoc])

;; main loading event
;; FIX: Event?
(tc type "string[]")
(local event [:BufReadPost :BufNewFile])

;; parens
(tc type string)
(local _all-parens "[%(%)%{%}%[%]]")
(tc type string)
(local _opening-parens "[%(%{%[]")

(tc param tag string)
(λ non-lisp-rules [tag]
  (local Rule (require :nvim-autopairs.rule))
  (local cond (require :nvim-autopairs.conds))
  (let [{: lisp-ft?} (require :hondana-dev.utils)]
    (-> tag
        (Rule tag) ; same open/close tag
        (: :with_pair (cond.not_before_regex_check "%w"))
        (: :with_pair #(not (lisp-ft? vim.bo.filetype))))))

(tc param line string return string)
(λ get-closing-for-line [line]
  (var i nil)
  (set i -1)
  (var clo "")
  (while true
    ;; (let [(n _) (string.find line _all-parens (+ 1 i))]
    ;;   (set i n))
    (set i (string.find line _all-parens (+ 1 i)))
    (when (not i) (lua :break))
    (let [ch (string.sub line i i)
          st (string.sub clo 1 1)]
      (if (= "{" ch)
          (set clo (.. "}" clo))
          (if (= "}" ch)
              (do
                (when (not= "}" st) (lua "return \"\""))
                (set clo (string.sub clo 2)))
              (if (= "(" ch)
                  (set clo (.. ")" clo))
                  (if (= ")" ch)
                      (do
                        (when (not= ")" st)
                          (lua "return \"\""))
                        (set clo (string.sub clo 2)))
                      (if (= "[" ch)
                          (set clo (.. "]" clo))
                          (if (= "]" ch)
                              (do
                                (when (not= "]" st) (lua "return \"\""))
                                (set clo (string.sub clo 2)))))))))))
  clo)

(local _ get-closing-for-line)

(tc type "LazyUrlConfig[]")
(local url-config-specs
       [:tommcdo/vim-exchange
        [:kylechui/nvim-surround
         #(let [{: setup} (require :nvim-surround)]
            (setup $2))]
        ;;; FIXME: removed because collides with nvim-treesitter-textobjects
        ;;; TODO: test which one
        ;; [:echasnovski/mini.ai
        ;;  ;; expect overridden nvim-surround combo keys
        ;;  #(let [{: setup} (require :mini.ai)]
        ;;     (setup {:n_lines 500}))]
        [:echasnovski/mini.surround
         #(let [{: setup} (require :mini.surround)]
            (setup $2))]
        [:opdavies/toggle-checkbox.nvim
         #(let [{:nvim_create_user_command uc
                 :nvim_create_autocmd au
                 :nvim_create_augroup augroup} vim.api
                group (augroup :Hondana_ToggleCheckbox {:clear true})
                callback #(vim.keymap.set :n :gt vim.cmd.ToggleCheckbox
                                          {:desc "Toggle Checkbox in text/md/org files"})
                pattern checkbox-text-fts]
            (uc :ToggleCheckbox ":lua require('toggle-checkbox').toggle()" {})
            (au :FileType {: callback : group : pattern}))]
        ;; TEST: a new treesitter-based paredit (used in fennel)
        ;; NOTE: put this code at the end (usage of `unpack`)
        (if (not= :julienvincent paredit-version)
            [:kovisoft/paredit
             #(do
                (local {:nvim_create_autocmd au :nvim_create_augroup augroup}
                       vim.api)
                (set vim.g.paredit_matchlines 300)
                ;; HACK: fennel & scheme have no Vim syntax but Treesitter only
                ;; => solution: syntax=lisp to avoid comment/string parens' matching
                (let [group (augroup :KovisoftParedit_NoSyntaxHack
                                     {:clear true})
                      callback #(vim.schedule #(set! :syntax :lisp))]
                  (au :FileType
                      {: callback : group :pattern [:fennel :scheme :query]}))
                ;; tressitter's query filetype needs paredit too
                ;; FIXME: sometimes doesn't work (especially when typing `o` after `:TreeInspect`
                (let [group (augroup :KovisoftParedit_Enable_Query
                                     {:clear true})
                      callback #(vim.cmd "cal PareditInitBuffer()")]
                  (au :FileType {: callback : group :pattern :query}))
                ;; fancy keybindings
                ;; <> : move left (like <leader><)
                ;; >< : move right (like <leader>>)
                (each [direction keys (pairs {:Left "<>" :Right "><"})]
                  (vim.keymap.set :n keys
                                  (-> direction
                                      (#["<Cmd>:call PareditMove" $ "()<CR>"])
                                      (table.concat)))))]
            ;; :else (cleaner setup but incomplete: no autospacing, electric return, smooth deletion with autopairs...
            (unpack [[:julienvincent/nvim-paredit
                      #(let [{: setup} (require :nvim-paredit)]
                         (setup {:dragging {:auto_drag_pairs false}
                                 :keys {;; NOTE: gE from tangerine.nvim was `:FnlBuffer`; it's now gB
                                        }}))]
                     [:windwp/nvim-autopairs
                      ;; NOTE: enable if julienvincent/nvim-paredit is too
                      #(let [{: setup : add_rule : remove_rule} (require :nvim-autopairs)]
                         (setup {:disable_filetype [:TelescopePrompt
                                                    :minifiles
                                                    :vim]
                                 ;; FIX: disable if annoying by uncomment the next line
                                 ;; :enable_check_bracket_line false
                                 })
                         ;; lisp exceptions for quotes & backticks
                         (each [_ char (ipairs ["'" "`"])]
                           (remove_rule char)
                           (add_rule (non-lisp-rules char))))]]))])

(tc param spec "LazyUrlConfig[]" return "LazySpec[]")
(fn make-lazyspec [spec]
  (icollect [_ pkg (ipairs spec)]
    (let [seq? (-> pkg (type) (= :table))
          url (if seq? (. pkg 1) pkg)
          spec {1 url : event}]
      (when seq?
        (set spec.config (. pkg 2)))
      spec)))

(tc type "LazySpec[]")
(local P (make-lazyspec url-config-specs))

;; NOTE: matchup replaces matchit (which was disabled from `boot.fnl`)
;;       check hondana-dev.plugins.treesitter for additional settings
(let [VM ;;
      {1 :andymass/vim-matchup
       :event :BufReadPost
       :config #(let [{: setup} (require :match-up)]
                  (setup $2))
       ;; check: `:let g:matchup_matchparen_offscreen`
       :opts {:matchparen {:offscreen {:method :popup}}}}]
  ;; TODO: experimental!
  ;; matchup is quite heavy so you can remove the `:event` key 
  ;; the plugin will load after 7 seconds but you'll have to
  ;; reload the file or make a new buffer
  (when (not VM.event)
    (set VM.init #(let [{: load} (require :lazy.core.loader)
                        {:plugins {:vim-matchup plugin}} (require :lazy.core.config)]
                    (when plugin
                      (vim.defer_fn #(load plugin {:start :start})
                        7)))))
  (table.insert P VM))

;; NOTE: only for the `:EasyAlign` command as previously used in my old $VIMRC for Vim 7.3+
(table.insert P {1 :junegunn/vim-easy-align :cmd :EasyAlign})

(tc alias FnConfig "| fun(self:LazyPlugin, opts:table)" "| true")
(tc alias LazyUrlConfig "{[1]:string, [2]:FnConfig} | string")

P

;;; DOC

;; VIM-EXCHANGE
;; swap two words easily
;; examples
;    cxiw => first time on the word A, prepare A for swapping
;            second time on a second word B, swap A and B

;; AUTOPAIRS (temporarily enabled with julienvincent/nvim-paredit)
;  INFO: I'd like a magic tool that closes all the treesitter brackets
;         when we want instead of a stupid pairing tool

;; NVIM-SURROUND (my brain is accustomed to these keybindings but this
;; plugin might be definitively replaced by MINI.SURROUND (see below)
;; classic quotes/brackets manipulation; eg: cs'" => change surroundings
;; additional text objects; eg: ; REM ((
;  cin) => change inners of the parens (cursor out)
;  da,  => delete between commas (cursor in)
;  d2i) => change inners including outer parens (cursor in)
;

;; MINI.AI
;; better around/inside textobjects (a bit like wellle/brackets.vim)
;  va)   => visually select around parentheses
;  yinq  => yank inside next quote
;  ci'   => change inside quotes
;; TODO: check for conflicts with nvim-surround
;; NOTE: `noice.nvim` (unused) may introduce weird messages when both
;;       plugins (nvim-surround & mini.ai) are activated
;

;; MINI.SURROUND
;; "better" vim-surround based on the `s` keys from `machakann/vim-sandwich`
;  saiw) => surround add inner word parentheses
;  sd',  => surround delete single quotes
;  sr)'  => surround replace parentheses by single quotes
;

;; TOGGLE-CHECKBOX
;; toggle a markdown/org box; [ ] becomes [x]; [x] becomes [ ]; append [ ] 
;; if nothing while respecting the list tag (say, `-`)
;  gt : toggle checkbox (useful in markdown/org)
;
;;   - pros: lightweight, no need mkdnflow or any specific tool
;;   - cons: no multi-switch, no way to customize the checked_character `x`
;

;; PAREDIT
;; s-expression editing = paredit as recommended by monkoose in the nvlime-tutor
;; - version julienvincent (currently inactive)
;  ,@ : splice sexp (unwrap around cursor; `,` = localleader)
;  >) : slurp forward
;  >( : barf backward
;  >e : drag element forwards (useful for fennel/clojure pairs between {})
;  ,o : raise form
;  ,O : raise element
;
;;   - pros
;;     - maintained Lua code
;;     - Treesitter
;;   - cons
;;     - one must implement at least autospacing & electric return
;;       like in kovisoft/paredit using nvim-autopairs (WIP)
;; 
;; - version kovisoft (old; currently active)
;  <>/>< : paredit move left/right
;
;;   - pros
;;     - autospacing before new inner opening parens (great for all lisp!)
;;     - easy deletion (when the matching is not broken)
;;     - `electric return` (great to expand the code during editing;
;;       add a return before closing parens; merge them with
;;       `vim.lsp.buf.format`)
;;   - cons
;;     - pairs easily broken (need parens in comments sometimes)
;;     - (minor!) cursor must be on the parens to slurp/barf (but one
;;       only need two keys instead of four)
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
;; s-expression editing; eg:
;    <( => barfing backward
;    >) => slurping forward
;  {1 :julienvincent/nvim-paredit
;   :event :BufReadPost
;   :opts {:extension {:fennel {:get_node_root :return}}}}

;;; DANGER! ;; targets ruin the Vim macros (recorded/typed)
;;; DANGER! ;; {1 :wellle/targets.vim}
