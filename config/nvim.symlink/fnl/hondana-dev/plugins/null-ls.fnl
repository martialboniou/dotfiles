;;; Additional Formatters, Diagnostic tools and Spellchecking
;;; 2023-11-06

(local unpack (or table.unpack _G.unpack))

(local mason-null-ls-preferred-install [:stylua :jq :ocamlformat :clang-format])

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

;; unused
(λ make-clang-generator-opts [factory]
  (let [command :/opt/homebrew/opt/llvm/bin/clang-format
        _ factory
        args [(.. :--style= "\"{BasedOnStyle: LLVM, IndentWidth: 4}\"")
              :--assume-filename=$FILENAME]]
    {: command : args :to_stdin true}))

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

(λ on_attach [client bufnr]
  "Disable the LSP formatting support when Null-ls is on"
  (when (client.supports_method :textDocument/formatting)
    (let [group (vim.api.nvim_create_augroup :LspFormatting {})
          buffer bufnr
          callback #(vim.lsp.buf.format {: bufnr})]
      (vim.api.nvim_clear_autocmds {: group : buffer})
      (vim.api.nvim_create_autocmd :BufWritePre {: group : buffer : callback}))))

{1 :jay-babu/mason-null-ls.nvim
 :event [:BufReadPost :BufNewFile]
 :dependencies [:williamboman/mason.nvim
                {1 :jose-elias-alvarez/null-ls.nvim
                 :dependencies [:nvim-lua/plenary.nvim]
                 :opts (λ []
                         (let [nls (require :null-ls)
                               h (require :null-ls.helpers)
                               diagno nls.builtins.diagnostics
                               format nls.builtins.formatting
                               ;; formatter for fennel; install https://git.sr.ht/~technomancy/fnlfmt
                               ;; NOTE: use `;; fnlfmt: skip` to skip the following sexp (snippet added)
                               fennel-formatter (h.make_builtin {:name :fennel-formatter
                                                                 :method nls.methods.FORMATTING
                                                                 :filetypes [:fennel]
                                                                 :generator_opts fnlfmt-command-pattern
                                                                 :factory h.formatter_factory})
                               ;; TESTING ONLY: clang-format (removed in the next commit)
                               clang-formatter (h.make_builtin {:name :clang_format
                                                                :method [nls.methods.FORMATTING
                                                                         nls.methods.RANGE_FORMATTING]
                                                                :ignore_stderr false
                                                                :filetypes [:c
                                                                            :cpp
                                                                            :cxx
                                                                            :cs
                                                                            :java
                                                                            :cuda
                                                                            :proto]
                                                                :generator_opts (-> h.range_formatting_args_factory
                                                                                    (make-clang-generator-opts))
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
                                      ;;; (when (-> clang-format-global-file
                                      ;;;           (vim.fn.filereadable) (= 1))
                                      ;;;   (format.clang_format.with {:extra_args (->> clang-format-global-file
                                      ;;;                                               (.. "--style=file:")
                                      ;;;                                               (#[$]))}))
                                      ;;; TESTING only: clang-formatter
                                      ;; great bonus for Fennel
                                      fennel-formatter
                                      ;; (go) :Mason install: gofumpt, goimports_reviser & golines
                                      ;; format.gofumpt           ;; add me when you go
                                      ;; format.goimports_reviser ;; add me when you go
                                      ;; format.golines           ;; add me when you go
                                      ;; (PHP/Symfony) :Mason install: php-cs-fixer & phpactor
                                      (format.phpcsfixer.with {:extra_args ["--rules=@PhpCsFixer,@Symfony"]})
                                      ;; bonus for Symfony
                                      twig-formatter
                                      ;; diagnostic test: warn really in markdown
                                      warn-really-in-markdown]
                            ;; : on_attach
                            :debug true}))}]
 :opts {:ensure_installed mason-null-ls-preferred-install
        :automatic_installation false}}
