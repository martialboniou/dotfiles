;;; table structure by: https://github.com/MuhametSmaili/nvim/blob/main/lua/smaili/plugins/lsp/init.lua
;;; LSP Zero version + fennel
;;; 2023-08-05
(local zero-setup-preferred-preset :recommended)
(local mason-null-ls-preferred-install [:stylua :jq])

(local lsp-custom-keymaps
       {:n {:<leader>f #(vim.lsp.buf.format)
            :gd #(vim.lsp.buf.definition)
            :K #(vim.lsp.buf.hover)
            :<leader>vws #(vim.lsp.buf.workspace_symbol)
            :<leader>vd #(vim.diagnostic.open_float)
            "[d" #(vim.diagnostic.goto_next)
            "]d" #(vim.diagnostic.goto_prev)
            :<leader>vca #(vim.lsp.buf.code_action)
            :<leader>vrr #(vim.lsp.buf.references)
            :<leader>vrn #(vim.lsp.buf.rename)
            ;;; ! for ergonomics: <leader> + ca = vca, rr = vrr, nn, vrn
            :<leader>ca #(vim.lsp.buf.code_action)
            :<leader>rr #(vim.lsp.buf.references)
            :<leader>nn #(vim.lsp.buf.rename)}
        :i {:<C-h> #(vim.lsp.buf.signature_help)}})

(local cmp-config-complete-options "menu,menuone,noinsert")

;; BEWARE: <Tab> is not available
(local cmp-custom-keymaps {:previous-word :<C-p>
                           :next-word :<C-n>
                           :complete :<C-Space>
                           :confirm :<C-y>
                           :abort :<C-e>})

;; NOTE: <C-f>/<C-b> = jump forward/backward (<C-b> CANNOT be used in tmux; I chose <M-a>)
;;       <C-u>/<C-d> = scroll

;; NOTE: stylua is ready to use but still unused here
(local mason-lspconfig-preferred-install
       [:rust_analyzer
        ;; codelldb
        ;; taplo
        ])

(local preferred-language-servers
       [:tsserver
        :rust_analyzer
        :html
        :lua_ls
        :jsonls
        :tailwindcss
        :dockerls
        :docker_compose_language_service
        :astro
        :vimls
        :cssls
        :fennel_language_server])

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

[;;; LSP
 {1 :neovim/nvim-lspconfig
  :event :BufReadPost
  :dependencies [;; LSP Zero
                 {1 :VonHeikemen/lsp-zero.nvim :branch :v2.x :lazy true}
                 ;; {1 :simrat39/rust-tools.nvim}
                 {1 :williamboman/mason.nvim
                  :opts {}
                  :cmd :Mason
                  :run ":MasonUpdate"}
                 {1 :williamboman/mason-lspconfig.nvim
                  :opts (λ [_ opts]
                          (when (= (type opts.ensure_installed) :table)
                            (vim.list_extend opts.ensure_installed
                                             mason-lspconfig-preferred-install)))}
                 ;; see Autocompletion
                 :hrsh7th/nvim-cmp
                 ;; optional/highlight same word (LSP support)
                 :rrethy/vim-illuminate
                 {;; optional/fancy navbar with LSP (+ other tools)
                  1 :glepnir/lspsaga.nvim
                  :dependencies [:nvim-tree/nvim-web-devicons
                                 :nvim-treesitter/nvim-treesitter]
                  :event :LspAttach
                  :opts {:code_action {:show_server_name true
                                       :extend_gitsigns false}
                         :lightbulb {:enable false}
                         :diagnostic {:on_insert false :on_insert_follow false}
                         :rename {:in_select false}}}]
  :opts {:diagnostics {:update_in_insert false :virtual_text true}
         :autoformat true
         :zero-setup {:preset zero-setup-preferred-preset
                      :preferences {:suggest_lsp_servers false}
                      :servers preferred-language-servers
                      :sign-icons {:error "✘"
                                   :warn "▲"
                                   :hint "⚑"
                                   :info "»"}
                      :sign-chars {:error :E :warn :W :hint :H :info :I}}}
  :config (λ [_ opts]
            ;; reduce boilerplate code with LSP Zero
            (local lsp-zero (require :lsp-zero))
            (local lsp-zero-setup opts.zero-setup)
            (lsp-zero.nvim_workspace)
            (lsp-zero.preset lsp-zero-setup.preset)
            (lsp-zero.ensure_installed lsp-zero-setup.servers)
            (lsp-zero.set_preferences lsp-zero-setup.preferences)
            ;; change the following to lsp-zero-setup.sign-chars
            (lsp-zero.set_sign_icons lsp-zero-setup.sign-icons)
            (vim.diagnostic.config opts.diagnostics)
            (lsp-zero.on_attach (λ [_ bufnr]
                                  (local options {:buffer bufnr :remap false})
                                  (each [mode map (pairs lsp-custom-keymaps)]
                                    (each [key fun (pairs map)]
                                      (vim.keymap.set mode key fun options)))))
            (local lspconfig (require :lspconfig))
            ;; Lua
            (lspconfig.lua_ls.setup (lsp-zero.nvim_lua_ls))
            (tset (require :lspconfig.configs) :fennel_language_server
                  {:default_config {:cmd [:fennel-language-server]
                                    :filetypes [:fennel]
                                    :single_file_support true
                                    :root_dir (lspconfig.util.root_pattern :fnl)
                                    :settings {:fennel {:workspace {:library (vim.api.nvim_list_runtime_paths)}
                                                        :diagnostics {:globals [:vim]}}}}})
            (lspconfig.fennel_language_server.setup {})
            (lsp-zero.setup))}
 ;;
 ;;; Autocompletion
 {1 :hrsh7th/nvim-cmp
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
 ;;
 ;;; Additional Formatters, Diagnostic tools and Spellchecking
 {1 :jay-babu/mason-null-ls.nvim
  :event [:BufReadPost :BufNewFile]
  :dependencies [:williamboman/mason.nvim
                 {1 :jose-elias-alvarez/null-ls.nvim
                  :dependencies [:nvim-lua/plenary.nvim]
                  :opts (λ []
                          (local nls (require :null-ls))
                          (local h (require :null-ls.helpers))
                          ;; formatter for fennel; install https://git.sr.ht/~technomancy/fnlfmt
                          ;; NOTE: use `;; fnlfmt: skip` to skip the following sexp (snippet added)
                          (local fennel-formatter
                                 (h.make_builtin {:name :fennel-formatter
                                                  :method nls.methods.FORMATTING
                                                  :filetypes [:fennel]
                                                  :generator_opts {:command :fnlfmt
                                                                   :args [:--fix
                                                                          :$FILENAME]
                                                                   :to_stdin false
                                                                   :to_temp_file true}
                                                  :factory h.formatter_factory}))
                          ;; formatter for Twig/Nunjucks template
                          (local twig-formatter
                                 (h.make_builtin {:name :twig-formatter
                                                  :method nls.methods.FORMATTING
                                                  :filetypes [:html.twig.js.css]
                                                  :generator_opts {:command :djlint
                                                                   :args [;; MANDATORY
                                                                          :--no-function-formatting
                                                                          :--profile=nunjucks
                                                                          :--max-blank-lines=1
                                                                          ;; the thing
                                                                          :--reformat
                                                                          :$FILENAME]
                                                                   :to_stdin false
                                                                   :to_temp_file true}
                                                  :factory h.formatter_factory}))
                          ;; FIXME: just here for testing
                          (local warn-really-in-markdown
                                 (let [really-gen (λ [params]
                                                    (local diagnostics {})
                                                    (each [i line (ipairs params.content)]
                                                      (local (col end-col)
                                                             (line:find :really))
                                                      (and col end-col
                                                           (table.insert diagnostics
                                                                         {: col
                                                                          :row i
                                                                          :end_col (+ end-col
                                                                                      1)
                                                                          :source :no-really
                                                                          :message "Don't use 'really!'"
                                                                          :severity vim.diagnostic.severity.WARN})))
                                                    diagnostics)]
                                   (h.make_builtin {:method nls.methods.DIAGNOSTICS
                                                    :filetypes [:markdown]
                                                    :generator {:fn really-gen}})))
                          {:sources [;; NOTE: the LSP Lua server is good for comments' alignment;
                                     ;;       if you prefer stylua, :MasonInstall stylua and
                                     ;;       uncomment the following line
                                     ; null-ls.builtins.formatting.stylua
                                     nls.builtins.diagnostics.eslint
                                     nls.builtins.diagnostics.twigcs
                                     ;; (FIXME: restore me) nls.builtins.completion.spell
                                     ;; (PHP/Symfony) :Mason install: php-cs-fixer & phpactor
                                     (nls.builtins.formatting.phpcsfixer.with {:extra_args ["--rules=@PhpCsFixer,@Symfony"]})
                                     ;; (go) :Mason install: gofumpt, goimports_reviser & golines
                                     ;; null-ls.builtins.formatting.gofumpt ;; add me when you go
                                     ;; null-ls.builtins.formatting.goimports_reviser ;; add me when you go
                                     ;; null-ls.builtins.formatting.golines ;; add me when you go
                                     fennel-formatter
                                     ;; great bonus for Fennel
                                     twig-formatter
                                     ;; bonus for Symfony
                                     warn-really-in-markdown
                                     ;; diagnostic test: warn really in markdown
                                     ]
                           :debug true})}]
  :opts {:ensure_installed mason-null-ls-preferred-install
         :automatic_installation false}}]
