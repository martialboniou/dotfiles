;;; Additional Formatters, Diagnostic tools and Spellchecking
;;; 2023-08-05

(local mason-null-ls-preferred-install [:stylua :jq :ocamlformat])

;; NOTE: ocamlformat requires a `.ocamlformat` file at the root of a dune project

(local fnlfmt-command-pattern
       {:command :fnlfmt
        :args [:--fix :$FILENAME]
        :to_stdin false
        :to_temp_file true})

(local djlint-command-pattern-for-twig
       {:command :djlint
        :args [;; MANDATORY
               :--no-function-formatting
               :--profile=nunjucks
               :--max-blank-lines=1
               ;; the thing
               :--reformat
               :$FILENAME]
        :to_stdin false
        :to_temp_file true})

(local markdown-really-diagnostics-generator
       (λ [params]
         (local diagnostics {})
         (each [i line (ipairs params.content)]
           (local (col end-col) (line:find :really))
           (and col end-col
                (table.insert diagnostics
                              {: col
                               :row i
                               :end_col (+ end-col 1)
                               :source :no-really
                               :message "Don't use 'really!'"
                               :severity vim.diagnostic.severity.WARN})))
         diagnostics))

{1 :jay-babu/mason-null-ls.nvim
 :event [:BufReadPost :BufNewFile]
 :dependencies [:williamboman/mason.nvim
                {1 :jose-elias-alvarez/null-ls.nvim
                 :dependencies [:nvim-lua/plenary.nvim]
                 :opts (λ []
                         (let [nls (require :null-ls)
                               h (require :null-ls.helpers)
                               ;; formatter for fennel; install https://git.sr.ht/~technomancy/fnlfmt
                               ;; NOTE: use `;; fnlfmt: skip` to skip the following sexp (snippet added)
                               fennel-formatter (h.make_builtin {:name :fennel-formatter
                                                                 :method nls.methods.FORMATTING
                                                                 :filetypes [:fennel]
                                                                 :generator_opts fnlfmt-command-pattern
                                                                 :factory h.formatter_factory})
                               ;; formatter for Twig/Nunjucks template
                               twig-formatter (h.make_builtin {:name :twig-formatter
                                                               :method nls.methods.FORMATTING
                                                               :filetypes [:html.twig.js.css]
                                                               :generator_opts djlint-command-pattern-for-twig
                                                               :factory h.formatter_factory})
                               ;; FIXME: just here for testing
                               warn-really-in-markdown (h.make_builtin {:method nls.methods.DIAGNOSTICS
                                                                        :filetypes [:markdown]
                                                                        :generator {:fn markdown-really-diagnostics-generator}})]
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
                            :debug true}))}]
 :opts {:ensure_installed mason-null-ls-preferred-install
        :automatic_installation false}}
