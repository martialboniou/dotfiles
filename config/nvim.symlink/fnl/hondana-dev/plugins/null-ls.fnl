;;; Additional Formatters, Diagnostic tools and Spellchecking
;;; 2024-03-23

(local mason-null-ls-preferred-install
       [:stylua
        :jq
        :ocamlformat
        :clang-format
        :gofumpt
        :goimports_reviser
        :golines])

;; NOTE: clangd lacks a way to customize its own clang-format (LLVM's indent width is 2; I want tabstop)
(local clang-format-indent-width
       (-> :tabstop
           (#(. vim.opt $))
           (: :get :value)))

;; RECOMMENDED: copy `~/.config/nvim/.clang-format` at the root of your C projects
;; ALTERNATE: you can enable a global clang-format configuration (and thus override
;;            every local `.clang-format` files) by setting the following to true
;; NOTE: delete `~/.config/nvim/.clang-format` when you change your tabstop,
;;       it will rebuild this file with your new setting
(local override-clang-format-globally false)

(local clang-format-global-file
       (-> :config (vim.fn.stdpath) (.. :/.clang-format) (vim.fn.expand)))

(when (-> clang-format-global-file (vim.fn.filereadable) (= 0))
  (let [file (io.open clang-format-global-file :w)
        options ["BasedOnStyle: LLVM"
                 "AlignArrayOfStructures: Right"
                 "AlignConsecutiveMacros:"
                 "  Enabled: true"
                 "  AcrossComments: false"
                 "  AcrossEmptyLines: true"
                 "BreakBeforeBraces: Custom"
                 "BraceWrapping:"
                 ;; I prefer BraceWrappingAfterFunction for C/C++; not for Zig
                 "  AfterFunction: true"
                 (->> clang-format-indent-width (.. "IndentWidth: "))]]
    (->> (icollect [_ line (ipairs options)] (.. line "\n"))
         (unpack)
         (..)
         (: file :write))
    (: file :close)))

;; NOTE: ocamlformat requires a `.ocamlformat` file at the root of a dune project

(local fnlfmt-command-pattern
       {:command :fnlfmt
        :args [:--fix :$FILENAME]
        :to_stdin false
        :to_temp_file true})

;; `gawk -o` is not great; TODO: find a better formatter
;; INFO: use `:retab` after `<leader>f`
(local gawk-command-pattern {:command :gawk
                             :args [:-o- :-f :$FILENAME]
                             :prepend_extra_args true
                             :to_stdin true
                             :to_temp_file false})

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

(λ markdown-really-diagnostics-generator [params]
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
  diagnostics)

{1 :jay-babu/mason-null-ls.nvim
 :event [:BufReadPost :BufNewFile]
 :dependencies [:williamboman/mason.nvim
                {1 :jose-elias-alvarez/null-ls.nvim
                 :dependencies [:nvim-lua/plenary.nvim]
                 :opts (λ []
                         (let [nls (require :null-ls)
                               h (require :null-ls.helpers)
                               b nls.builtins
                               diagno b.diagnostics
                               format b.formatting
                               ;; formatter for fennel; install https://git.sr.ht/~technomancy/fnlfmt
                               ;; NOTE: use `;; fnlfmt: skip` to skip the following sexp (snippet added)
                               fennel-formatter (h.make_builtin {:name :fennel-formatter
                                                                 :method nls.methods.FORMATTING
                                                                 :filetypes [:fennel]
                                                                 :generator_opts fnlfmt-command-pattern
                                                                 :factory h.formatter_factory})
                               ;; formatter for awk (gawk required)
                               awk-formatter (h.make_builtin {:name :awk-formatter
                                                              :method nls.methods.FORMATTING
                                                              :filetypes [:awk]
                                                              :generator_opts gawk-command-pattern
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
                                      ; format.stylua
                                      diagno.eslint
                                      diagno.twigcs
                                      ;; (FIXME: restore me) nls.builtins.completion.spell
                                      ;; clang-format with more options than with clangd in LSP
                                      ;;   ensure clangd.setup has capabilities.offsetEncoding
                                      ;;   set to [:utf-16] (check hondana-dev.plugins.lsp)
                                      (let [extras (if (and override-clang-format-globally
                                                            (-> clang-format-global-file
                                                                (vim.fn.filereadable)
                                                                (= 1)))
                                                       {:extra_args (->> clang-format-global-file
                                                                         (.. "--style=file:")
                                                                         (#[$]))}
                                                       {})]
                                        (format.clang_format.with extras))
                                      ;; great bonus for Fennel
                                      fennel-formatter
                                      awk-formatter
                                      ;; (go) :Mason install: gofumpt, goimports_reviser & golines
                                      format.gofumpt
                                      format.goimports_reviser
                                      format.golines
                                      ;; (PHP/Symfony) :Mason install: php-cs-fixer & phpactor
                                      (format.phpcsfixer.with {:extra_args ["--rules=@PhpCsFixer,@Symfony"]})
                                      ;; bonus for Symfony
                                      twig-formatter
                                      ;; diagnostic test: warn really in markdown
                                      warn-really-in-markdown]
                            :debug true}))}]
 :opts {:ensure_installed mason-null-ls-preferred-install
        :automatic_installation false}}
