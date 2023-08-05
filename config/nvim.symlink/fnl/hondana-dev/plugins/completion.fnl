;;; LSP Zero version
;;; 2023-08-05

(local cmp-config-complete-options "menu,menuone,noinsert")

;; BEWARE: <Tab> is not available
(local cmp-custom-keymaps {:previous-word :<C-p>
                           :next-word :<C-n>
                           :complete :<C-Space>
                           :confirm :<C-y>
                           :abort :<C-e>})

;; NOTE: <C-f>/<C-b> = jump forward/backward (<C-b> CANNOT be used in tmux; I chose <M-a>)
;;       <C-u>/<C-d> = scroll

(macro make-emoji-restrictions [...]
  "build the entry filter function"
  (let [clauses []
        context (gensym :context)]
    (each [_ ctxt (ipairs [...])]
      (let [c (string.lower ctxt)
            cap-c (string.gsub c "^%l" string.upper) ; gsub return 2 values
            syntaxes {:in_treesitter_capture c :in_syntax_group cap-c}]
        (each [f a (pairs syntaxes)]
          (table.insert clauses `(. ,context ,f ,a)))))
    `(fn []
       (let [,context (require :cmp.config.context)]
         (or ,(unpack clauses))))))

(local cmp-config-preferred-default-sources
       [{:name :nvim_lsp_signature_help}
        {:name :nvim_lsp}
        {:name :luasnip}
        {:name :buffer :keyword_length 4}
        {:name :path}
        {:name :emoji
         :entry_filter (make-emoji-restrictions :comment :string)
         :insert true}])

(local cmp-config-preferred-markdown-sources
       [{:name :nvim_lsp_signature_help}
        {:name :nvim_lsp}
        {:name :luasnip}
        {:name :buffer :keyword_length 3}
        {:name :path}
        {:name :emoji :insert true}])

(local cmp-config-preferred-git-sources
       [{:name :nvim_lsp_signature_help}
        {:name :nvim_lsp}
        {:name :luasnip}
        {:name :buffer :keyword_length 4}
        {:name :path}
        {:name :git}
        {:name :emoji :insert true}])

(local cmp-config-preferred-modern-lisp-sources
       [{:name :nvim_lsp_signature_help}
        {:name :nvim_lsp}
        ;; cmp-conjure!
        {:name :conjure}
        {:name :luasnip}
        {:name :buffer :keyword_length 4}
        {:name :path}
        {:name :emoji
         :entry_filter (make-emoji-restrictions :comment)
         :insert true}])

{1 :hrsh7th/nvim-cmp
 ;; check lsp.fnl for the loading LSP Zero -> LSP -> nvim-cmp
 :dependencies [:hrsh7th/cmp-nvim-lsp
                {1 :L3MON4D3/LuaSnip
                 :build "make install_jsregexp"
                 ;; optional: friendly-snippets
                 :dependencies [:rafamadriz/friendly-snippets]
                 :config #(#($.lazy_load {:paths (.. (vim.fn.stdpath :config)
                                                     :/snippets/vscode)}) (require :luasnip.loaders.from_vscode))}
                ;; optional/conjure plugin
                :PaterJason/cmp-conjure
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
 :opts (λ []
         ;; nvim-lspconfig ensures the lazy loading of LSP Zero
         (local lsp-zero-cmp (require :lsp-zero.cmp))
         (lsp-zero-cmp.extend)
         (local cmp (require :cmp))
         ;; (local cmp-action-from-lsp-zero (lsp-zero-cmp.action))
         ;; OLD: -- vim.opt.runtimepath:apped("~/github/lsp_signature.nvim")
         (local lsp-zero (require :lsp-zero))
         (cmp.setup.filetype [:markdown]
                             {:sources (cmp.config.sources cmp-config-preferred-markdown-sources)})
         (cmp.setup.filetype [:gitcommit]
                             {:sources (cmp.config.sources cmp-config-preferred-git-sources)})
         (cmp.setup.filetype [:fennel :clojure]
                             {:sources (cmp.config.sources cmp-config-preferred-modern-lisp-sources)})
         {:completion {:completeopt cmp-config-complete-options}
          :window {:completion (cmp.config.window.bordered)
                   :documentation (cmp.config.window.bordered)}
          :snippet {:expand (λ [args]
                              (#($.lsp_expand args.body) (require :luasnip)))}
          :sources (cmp.config.sources cmp-config-preferred-default-sources)
          ;; :experimental {:ghost_text true}
          :mapping (lsp-zero.defaults.cmp_mappings {(or cmp-custom-keymaps.previous-word
                                                         :<C-p>) (cmp.mapping.select_prev_item {:behavior cmp.SelectBehavior.Select})
                                                    (or cmp-custom-keymaps.next-word
                                                         :<C-n>) (cmp.mapping.select_next_item {:behavior cmp.SelectBehavior.Select})
                                                    (or cmp-custom-keymaps.complete
                                                         :<C-Space>) (cmp.mapping.complete)
                                                    (or cmp-custom-keymaps.confirm
                                                         :<C-y>) (cmp.mapping.confirm {:select true})
                                                    (or cmp-custom-keymaps.abort
                                                         :<C-e>) (cmp.mapping.abort)
                                                    :<Tab> nil
                                                    :<S-Tab> nil})})}
