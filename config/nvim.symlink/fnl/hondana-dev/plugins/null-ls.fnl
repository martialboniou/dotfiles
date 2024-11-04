;;; Additional Formatters, Diagnostic tools and Spellchecking
;;; 2024-11-04
(import-macros {: tc} :hondana-dev.macros)

;; same as mason.setup.ensure_installed
(tc type "string[]")
(local mason-null-ls-preferred-install
       [:stylua
        :jq
        ;; taplo = toml toolkit used by some zk functions (see hondana-dev.utils)
        :taplo
        :ocamlformat
        :clang-format
        :awk-language-server
        :markdownlint-cli2
        :markdown-toc
        :gofumpt
        :goimports_reviser
        :golines])

;; NOTE: clangd lacks a way to customize its own clang-format (LLVM's indent width is 2; I want tabstop)
(tc type number)
(local clang-format-indent-width
       (-> :tabstop
           (#(. vim.opt $))
           (: :get :value)))

;; RECOMMENDED: copy `~/.config/nvim/.clang-format` at the root of your C projects
;; ALTERNATE: you can enable a global clang-format configuration (and thus override
;;            every local `.clang-format` files) by setting the following to true
;; NOTE: delete `~/.config/nvim/.clang-format` when you change your tabstop,
;;       it will rebuild this file with your new setting
(tc type boolean)
(local override-clang-format-globally false)

(tc type string)
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
    (when file
      (->> (icollect [_ line (ipairs options)] (.. line "\n"))
           (unpack)
           (..)
           (file:write))
      (file:close))))

;; NOTE: ocamlformat requires a `.ocamlformat` file at the root of a dune project

(tc alias command_pattern
    "{command: string, args: string[], to_stdin: boolean, to_temp_file: boolean}"
    type command_pattern)

(local fnlfmt-command-pattern
       {:command :fnlfmt
        :args [:--fix :$FILENAME]
        :to_stdin false
        :to_temp_file true})

;; `gawk -o` is not great; TODO: find a better formatter
;; INFO: use `:retab` after `<leader>f`
(tc type command_pattern)
(local gawk-command-pattern {:command :gawk
                             :args [:-o- :-f :$FILENAME]
                             ; :prepend_extra_args true
                             :to_stdin true
                             :to_temp_file false})

(tc type command_pattern)
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

;; TODO: WIP
(lua "--- from vim.diagnostic")

;; fnlfmt: skip
(tc class vim.Diagnostic
    field bufnr? integer
    field lnum integer field
    end_lnum? integer
    field col integer
    field end_col? integer
    field severity? any vim.diagnostic.Severity 
    field message string 
    field source? string 
    field code? :string|integer
    field _tags? "{deprecated: boolean, unnecessary: boolean}"
    field user_data? any arbitrary data plugins can add
    field namespace? integer)

;; (tc type
;;     "fun(params: any): {col: number, row: number, end_col: number, source: string, message: string, severity?: vim.diagnostic.SeverityFilter}")

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

(tc type LazySpec)
(local P ;;
       {1 :jay-babu/mason-null-ls.nvim
        :event [:BufReadPost :BufNewFile]
        :dependencies [:williamboman/mason.nvim
                       {1 :jose-elias-alvarez/null-ls.nvim
                        :dependencies [:nvim-lua/plenary.nvim]
                        :opts (λ []
                                (let [{:builtins {:diagnostics diagno
                                                  :formatting format}
                                       : methods} (require :null-ls)
                                      {: make_builtin : formatter_factory} (require :null-ls.helpers)
                                      ;; formatter for fennel; install https://git.sr.ht/~technomancy/fnlfmt
                                      ;; NOTE: use `;; fnlfmt: skip` to skip the following sexp (snippet added)
                                      fennel-formatter (make_builtin {:name :fennel-formatter
                                                                      :method methods.FORMATTING
                                                                      :filetypes [:fennel]
                                                                      :generator_opts fnlfmt-command-pattern
                                                                      :factory formatter_factory})
                                      ;; formatter for awk (gawk required)
                                      awk-formatter (make_builtin {:name :awk-formatter
                                                                   :method methods.FORMATTING
                                                                   :filetypes [:awk]
                                                                   :generator_opts gawk-command-pattern
                                                                   :factory formatter_factory})
                                      ;; formatter for Twig/Nunjucks template
                                      twig-formatter (make_builtin {:name :twig-formatter
                                                                    :method methods.FORMATTING
                                                                    :filetypes [:html.twig.js.css]
                                                                    :generator_opts djlint-command-pattern-for-twig
                                                                    :factory formatter_factory})
                                      ;; FIXME: just here for testing
                                      warn-really-in-markdown (make_builtin {:method methods.DIAGNOSTICS
                                                                             :filetypes [:markdown]
                                                                             :generator {:fn markdown-really-diagnostics-generator}})]
                                  {:sources [;; NOTE: stylua: skip a block with `-- stylua: ignore start` until `-- stylua: ignore end`
                                             (let [extras {:extras_args {:indent_type :Spaces}}]
                                               (format.stylua.with extras))
                                             diagno.eslint
                                             diagno.twigcs
                                             ;; FIXME: restore me: builtins.completion.spell
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
               :automatic_installation false}})

P
