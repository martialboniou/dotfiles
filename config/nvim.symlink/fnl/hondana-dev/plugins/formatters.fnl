;;; Conform setup
;;; 2024-12-02
(import-macros {: tc} :hondana-dev/macros)

;; make a `.clang-format` at the root of this setup
(tc type string)
(local clang-format-global-file
       (-> :config (vim.fn.stdpath) (.. :/.clang-format)))

(when (-> clang-format-global-file (vim.fn.filereadable) (= 0))
  (let [file (io.open clang-format-global-file :w)
        ;; use the `tabstop` defined in `hondana-dev.set` as indent width (should be 4)
        indent-width (-> :tabstop (#(. vim.opt $)) (: :get :value))
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
                 (->> indent-width (.. "IndentWidth: "))]]
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

{1 :stevearc/conform.nvim
 :event [:BufWritePre]
 :cmd [:ConformInfo]
 :keys [{1 :<leader>f
         2 #(let [{: format} (require :conform)]
              (format {:async true :lsp_format :fallback}))
         :mode ""
         :desc "Format buffer (Conform)"}]
 :opts {;; WIP so debug ON
        :log_level vim.log.levels.DEBUG
        :formatters_by_ft {:fennel [:fnlfmt]
                           :lua [:stylua]
                           :c [:clang-format]
                           :cpp [:clang-format]
                           :go [:gofumpt :goimports-reviser :golines]
                           ;; ensure you have the good alias (I prefer gawk here)
                           :awk [:awk]
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
             ;; unless `.stylua.toml`
             (add_formatter_args (require :conform.formatters.stylua)
                                 ["--indent-type" :Spaces :--indent-width 2]
                                 false)
             ;; TODO: testing
             (add_formatter_args (require :conform.formatters.php_cs_fixer)
                                 ["--rules=@PhpCsFixer,@Symfony"] false)
             ;; `clang-format-global-file` may be used even if you have a `.clang-format`
             ;; (this option should be disabled by default)
             ;; NOTE: checked
             (when (and override-clang-format-globally
                        (-> clang-format-global-file (vim.fn.filereadable)
                            (= 1)))
               (-> "Formatting using " (.. clang-format-global-file)
                   (vim.notify vim.log.levels.INFO))
               (add_formatter_args (require :conform.formatters.clang-format)
                                   [(.. :--style=file: clang-format-global-file)]
                                   false))
             (setup opts)))}
