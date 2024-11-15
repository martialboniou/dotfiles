(import-macros {: tc} :hondana-dev/macros)
;;; LSP Zero version
;;; 2024-11-04

;; IMPORTANT: noselect = no <CR> in CMP = what I want here
(tc type :string)
(local cmp-config-complete-options "menu,menuone,noinsert, noselect")

;; IMPORTANT: <Tab>/<S-Tab> disabled in CMP
(tc type "table<string, string>")
(local cmp-custom-keymaps {:previous-word :<C-p>
                           :next-word :<C-n>
                           :complete :<C-Space>
                           :confirm :<C-y>
                           :abort :<C-e>})

;; NOTE: <C-f>/<C-b> = jump forward/backward (<C-b> CANNOT be used in tmux; I chose F5/`fn a` as a tmux prefix)
;;       <C-u>/<C-d> = scroll

(macro make-emoji-restrictions [...]
  "build the entry filter function"
  (let [clauses []
        context (gensym :context)]
    (each [_ ctxt (ipairs [...])]
      (let [c (string.lower ctxt)
            cap-c (string.gsub c "^%l" string.upper) ; gsub returns 2 values
            syntaxes {:in_treesitter_capture c :in_syntax_group cap-c}]
        (each [f a (pairs syntaxes)]
          (table.insert clauses `((. ,context ,f) ,a)))))
    `(λ []
       (let [,context (require :cmp.config.context)]
         (or ,(unpack clauses))))))

(tc type "table<number|string, source[]>")
(local cmp-config-preferred-sources
       {1 [{:name :nvim_lsp_signature_help}
           {:name :nvim_lsp}
           {:name :luasnip}
           {:name :buffer :keyword_length 4}
           {:name :path}
           {:name :emoji
            :entry_filter (make-emoji-restrictions :comment :string)
            :insert true}]
        :markdown [{:name :nvim_lsp_signature_help}
                   {:name :nvim_lsp}
                   {:name :luasnip}
                   {:name :buffer :keyword_length 3}
                   {:name :path}
                   {:name :emoji :insert true}]
        :gitcommit [{:name :nvim_lsp_signature_help}
                    {:name :nvim_lsp}
                    {:name :luasnip}
                    {:name :buffer :keyword_length 4}
                    {:name :path}
                    {:name :git}
                    {:name :emoji :insert true}]
        :lisp [{:name :nvim_lsp_signature_help}
               {:name :nvim_lsp}
               ;; cmp-conjure enabled by plugins.conjure
               {:name :conjure}
               {:name :luasnip}
               {:name :buffer :keyword_length 4}
               {:name :path}
               {:name :emoji
                :entry_filter (make-emoji-restrictions :comment)
                :insert true}]})

(tc param ?name string return "source[]")
(fn cmp-config [?name]
  (let [n (or ?name 1)]
    (-> cmp-config-preferred-sources
        (#(or (. $ n) (. $ 1))))))

(tc type :extract_fts)
(fn extract-fts [fts]
  (if (-> fts (type) (= :table))
      (if (not= nil (. fts :source))
          (values (. fts 1) (. fts :source))
          (values fts nil))
      ;; not a table
      (values fts fts)))

(tc type LazySpec)
(local P ;;
       {1 :hrsh7th/nvim-cmp
        ;; check lsp.fnl for the loading LSP Zero -> LSP -> nvim-cmp
        :dependencies [{1 :hrsh7th/cmp-nvim-lsp
                        :dependencies :neovim/nvim-lspconfig}
                       {1 :L3MON4D3/LuaSnip
                        :version :v2.*
                        :build "make install_jsregexp"
                        ;; optional: friendly-snippets
                        :dependencies [:rafamadriz/friendly-snippets]
                        :config #(let [{: lazy_load} (require :luasnip.loaders.from_vscode)]
                                   (lazy_load {:paths (-> :config
                                                          (vim.fn.stdpath)
                                                          (.. :/snippets/vscode))}))}
                       :saadparwaiz1/cmp_luasnip
                       ;; optional/buffer words
                       :hrsh7th/cmp-buffer
                       ;; optional/filesystem paths
                       :hrsh7th/cmp-path
                       ;; optional/git
                       :petertriho/cmp-git
                       ;; very optional (see sources.emoji)
                       {1 :hrsh7th/cmp-emoji
                        ;; :dependencies { :nvim-treesitter/nvim-treesitter }
                        }
                       :ray-x/lsp_signature.nvim
                       ;; :hrsh7th/cmp-nvim-lsp-signature-help ; replaced by previous
                       ]
        :opts (λ [_ _]
                ;; nvim-lspconfig ensures the lazy loading of LSP Zero
                (let [{: extend} (require :lsp-zero.cmp)] (extend))
                (let [{: setup
                       :SelectBehavior {:Select behavior}
                       :config cc
                       :mapping cm} (require :cmp)
                      {: defaults} (require :lsp-zero)]
                  ;; special sources
                  (each [_ fts (ipairs [:markdown
                                        :gitcommit
                                        {1 [:fennel :clojure] :source :lisp}])]
                    (let [(filetypes source) (extract-fts fts)
                          sources (cc.sources (cmp-config source))]
                      (setup.filetype filetypes {: sources})))
                  {:sources (cc.sources (cmp-config))
                   :completion {:completeopt cmp-config-complete-options}
                   :preselect :none
                   :snippet {:expand #(let [l (require :luasnip)]
                                        (l.lsp_expand $.body))}
                   :window {:completion (cc.window.bordered)
                            :documentation (cc.window.bordered)}
                   ;; :experimental {:ghost_text true}
                   :mapping (let [overrides {(or cmp-custom-keymaps.previous-word
                                                  :<C-p>) (cm.select_prev_item {: behavior})
                                             (or cmp-custom-keymaps.next-word
                                                  :<C-n>) (cm.select_next_item {: behavior})
                                             (or cmp-custom-keymaps.complete
                                                  :<C-Space>) (cm.complete)
                                             (or cmp-custom-keymaps.confirm
                                                  :<C-y>) (cm.confirm {:select true})
                                             (or cmp-custom-keymaps.abort
                                                  :<C-e>) (cm.abort)
                                             :<Tab> vim.NIL
                                             :<S-Tab> vim.NIL}
                                  mappings (defaults.cmp_mappings overrides)]
                              mappings)}))})

(tc alias bool_fun "fun(): boolean")
(tc alias source
    "{name: string, keyword_length?: number, entry_filter?: bool_fun, insert?: boolean}")

(tc alias fts ;;
    "| string" ;;
    "| {[1]: string[], source: nil|string}")

(tc alias :extract_fts ;;
    "| fun(fts: fts): string[], string" ;;
    "| fun(fts: fts): string[], nil" ;;
    "| fun(fts: fts): string, string")

P
