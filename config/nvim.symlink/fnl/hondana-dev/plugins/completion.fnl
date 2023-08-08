;;; LSP Zero version
;;; 2023-08-06 - FIX: <Tab> disabled

;; IMPORTANT: noselect = no <CR> in CMP = what I want here
(local cmp-config-complete-options "menu,menuone,noinsert, noselect")

;; IMPORTANT: <Tab>/<S-Tab> disabled in CMP
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
          (table.insert clauses `((. ,context ,f) ,a)))))
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
        {:name :conjure} ;; cmp-conjure enabled by plugins.conjure
        {:name :luasnip}
        {:name :buffer :keyword_length 4}
        {:name :path}
        {:name :emoji
         :entry_filter (make-emoji-restrictions :comment)
         :insert true}])

{1 :hrsh7th/nvim-cmp
 ;; check lsp.fnl for the loading LSP Zero -> LSP -> nvim-cmp
 :dependencies [{1 :hrsh7th/cmp-nvim-lsp :dependencies :neovim/nvim-lspconfig}
                {1 :L3MON4D3/LuaSnip
                 :build "make install_jsregexp"
                 ;; optional: friendly-snippets
                 :dependencies [:rafamadriz/friendly-snippets]
                 :config #(#($.lazy_load {:paths (.. (vim.fn.stdpath :config)
                                                     :/snippets/vscode)}) (require :luasnip.loaders.from_vscode))}
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
         {:sources (cmp.config.sources cmp-config-preferred-default-sources)
          :completion {:completeopt cmp-config-complete-options}
          :preselect :none
          :snippet {:expand (λ [args]
                              (#($.lsp_expand args.body) (require :luasnip)))}
          :window {:completion (cmp.config.window.bordered)
                   :documentation (cmp.config.window.bordered)}
          ;; :experimental {:ghost_text true}
          :mapping (let [overrides {(or cmp-custom-keymaps.previous-word
                                         :<C-p>) (cmp.mapping.select_prev_item {:behavior cmp.SelectBehavior.Select})
                                    (or cmp-custom-keymaps.next-word :<C-n>) (cmp.mapping.select_next_item {:behavior cmp.SelectBehavior.Select})
                                    (or cmp-custom-keymaps.complete :<C-Space>) (cmp.mapping.complete)
                                    (or cmp-custom-keymaps.confirm :<C-y>) (cmp.mapping.confirm {:select true})
                                    (or cmp-custom-keymaps.abort :<C-e>) (cmp.mapping.abort)
                                    :<Tab> vim.NIL
                                    :<S-Tab> vim.NIL}
                         mappings (lsp-zero.defaults.cmp_mappings overrides)]
                     mappings)})}
