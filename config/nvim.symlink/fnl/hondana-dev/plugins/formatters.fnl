;;; Conform setup
;;; 2025-05-21
(import-macros {: tc} :hondana-dev.macros)

(local {: fs :fn {: stdpath : filereadable}} vim)

;; S = additional settings
(local S {})

;; TODO: add a cache AND a command to add/remove these files
;; list of filetypes to format-on-save
(set S.format-on-save-filetypes [:fennel :exs :ex])
;; TODO: disable in certain path (tests, node_modules...)
;; (when (not (: (vim.api.nvim_buf_get_name $) :match "/node_modules/")) ?)

;;INFO: comment these two lines to NOT format on save AT ALL
(tc type "nil|conform.FormatOpts|fun(bufnr:integer): nil|conform.FormatOpts")
(set S.format_on_save
     #(when (->> $ (. vim.bo) (#(. $ :filetype))
                 (vim.tbl_contains S.format-on-save-filetypes))
        {:timeout_ms 500 :lsp_format :fallback}))

;; make a `.clang-format` at the root of this setup
(tc type string)
(set S.clang-format-global-file
     (-> :config (stdpath) (fs.joinpath :.clang-format)))

(when (-> S.clang-format-global-file (filereadable) (= 0))
  (let [file (io.open S.clang-format-global-file :w)
        options ["BasedOnStyle: LLVM"
                 "AlignArrayOfStructures: Right"
                 "AlignConsecutiveMacros:"
                 "  Enabled: true"
                 "  AcrossComments: false"
                 "  AcrossEmptyLines: true"
                 "BreakBeforeBraces: Custom"
                 "BraceWrapping:"
                 ;; BraceWrappingAfterFunction for C/C++ can be set to true here
                 "  AfterFunction: false"
                 ;; use the `tabstop` defined in `hondana-dev.set` as indent
                 ;; width (should be 4 for clang)
                 (->> :tabstop (vim.filetype.get_option :c)
                      (.. "IndentWidth: "))]]
    (when file
      (->> (icollect [_ line (ipairs options)] (.. line "\n"))
           (unpack)
           (..)
           (file:write))
      (file:close))))

;; RECOMMENDED: copy `~/.config/nvim/.clang-format` at the root of your C projects
;; ALTERNATE: you can enable a global clang-format configuration (and thus override
;;            every local `.clang-format` files) by setting the following to true
;; NOTE: delete `~/.config/nvim/.clang-format` when you change your tabstop,
;;       it will rebuild this file with your new setting
(tc type boolean)
(local override-clang-format-globally false)

(local pretty-fmt {1 :prettierd :stop_after_first true})

;; don't append `:--indent-width` here
(local stylua-extend-args [:--indent-type :Spaces])

(var default-stylua-args nil)

;;; PLUGINS
(tc type LazySpec)
(local P {1 :stevearc/conform.nvim
          :event [:BufWritePre]
          :cmd [:ConformInfo]
          :keys #(let [{: format} (require :conform)]
                   [{1 :<leader>f
                     2 #(format {:async true :lsp_format :fallback})
                     :mode ""
                     :desc "Format buffer (Conform)"}])
          :opts {;; WIP so debug ON
                 :log_level vim.log.levels.DEBUG
                 ;; check `hondana-dev.plugins.lsp` for the Mason current installs
                 :formatters_by_ft {:fennel [:fnlfmt]
                                    :lua [:stylua]
                                    :nix [:nixfmt]
                                    :javascript pretty-fmt
                                    :typescript pretty-fmt
                                    :css pretty-fmt
                                    ;; REMINDER: check the `fnl.hondana-dev.set` module
                                    ;; as `vim.g.markdown_recommended_style` is set to 0
                                    :markdown pretty-fmt
                                    :graphql pretty-fmt
                                    :astro (let [as pretty-fmt]
                                             (set as.lsp_format :fallback)
                                             as)
                                    :c [:clang-format]
                                    :cpp [:clang-format]
                                    :go [:gofumpt :goimports-reviser :golines]
                                    ;; ensure you have the good alias (I prefer gawk here)
                                    :awk [:awk]
                                    :json [:jq]
                                    :yaml [:yq]
                                    :sh [:shfmt]
                                    :rust [:rustfmt]
                                    :python [:isort :black]
                                    ;; HTML = lsp format
                                    :html {:lsp_format :prefer}
                                    :php [:php_cs_fixer]
                                    :twig [:djlint-twig]}
                 :formatters {;; custom formatter for twig
                              :djlint-twig {:meta {:url "https://github.com/Riverside-Healthcare/djLint"
                                                   :description "✨ HTML Template Linter and Formatter. Django - Jinja - Nunjucks - Handlebars - GoLang."}
                                            :command :djlint
                                            :args [:--no-function-formatting
                                                   :--profile=nunjucks
                                                   :--max-blank-lines=1
                                                   :--reformat
                                                   :-]}}}
          :config (fn [_ opts]
                    (let [{: add_formatter_args} (require :conform.util)
                          {: setup} (require :conform)]
                      (let [s (require :conform.formatters.stylua)]
                        ;; unless `.stylua.toml`
                        (add_formatter_args s stylua-extend-args)
                        ;; record the settings as `conform.util.extend_args()` will erase it
                        (set default-stylua-args
                             {:args s.args :range_args s.range_args})
                        (add_formatter_args s [:--indent-width :2]))
                      ;; TODO: testing
                      (add_formatter_args (require :conform.formatters.php_cs_fixer)
                                          ["--rules=@PhpCsFixer,@Symfony"])
                      ;; `S.clang-format-global-file` may be used even if you have a `.clang-format`
                      ;; (this option should be disabled by default)
                      ;; NOTE: checked
                      (when (and override-clang-format-globally
                                 (-> S.clang-format-global-file
                                     (filereadable)
                                     (= 1)))
                        (-> "Formatting using " (.. S.clang-format-global-file)
                            (vim.notify vim.log.levels.INFO))
                        (add_formatter_args (require :conform.formatters.clang-format)
                                            [(.. :--style=file:
                                                 S.clang-format-global-file)]))
                      (setup opts)))})

(when S.format_on_save
  (set P.opts.format_on_save S.format_on_save))

;;: OPTIONAL
;; force Stylua settings to reset according to your local option `shiftwidth` (use :Sleuth to autodetect it)
(let [{:nvim_create_augroup augroup
       :nvim_create_autocmd au
       :nvim_buf_create_user_command uc} vim.api
      pattern :lua
      group (augroup :Hondana_Conform_StyluaReset {})
      callback #(uc 0 :StyluaReset
                    #(do
                       (when (not default-stylua-args)
                         (vim.notify "cannot change reset the stylua formatter"
                                     vim.log.levels.ERROR)
                         (lua "return"))
                       (let [{: add_formatter_args} (require :conform.util)
                             stylua (require :conform.formatters.stylua)]
                         ;; first restore original setup
                         (set stylua.args default-stylua-args.args)
                         (set stylua.range_args default-stylua-args.range_args)
                         ;; then reapply changes with `shiftwidth` as `--indent-width`
                         (add_formatter_args stylua
                                             [:--indent-width
                                              (tostring vim.o.sw)])))
                    {})]
  (au :FileType {: group : callback : pattern}))

P
