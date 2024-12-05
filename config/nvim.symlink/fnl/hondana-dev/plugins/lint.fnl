;;; Lint setup
;;; 2024-12-02
(import-macros {: tc} :hondana-dev.macros)

;; F = utility functions at the end of this module
(local F {})

(local twigcs-bin :twigcs)
(local severity-map (icollect [_ label (ipairs [:INFO :WARN :ERROR :HINT])]
                      (. vim.diagnostic.severity label)))

;;; CONFIG
;; FIXME: lint.Linter.cmd should be string|function, not string (inform mathias)
(tc diagnostic "disable: assign-type-mismatch")
(fn config []
  (let [lint (require :lint)
        {:nvim_create_autocmd au :nvim_create_augroup augroup} vim.api
        group (augroup :Hondana_Lint {:clear true})
        callback #(when (vim.opt_local.modifiable:get)
                    (lint.try_lint))]
    (set lint.linters.twigcs
         {:name :twigcs
          ;; check for a local binary first
          :cmd #(let [bin twigcs-bin
                      local-bin (-> :vendor/bin/ (.. bin)
                                    (vim.fn.fnamemodify ":p"))]
                  (if (vim.uv.fs_stat local-bin)
                      local-bin
                      bin))
          :args [:--reporter :json]
          :ignore_exitcode true
          :parser F.twigcs-parser})
    (set lint.linters_by_ft {:javascript [:eslint]
                             :typescript [:eslint]
                             :json [:jq :jsonlint]
                             :markdown []
                             :awk [:gawk]
                             :cmake [:cmakelint]
                             ;; add credo to the deps of your mix
                             :elixir [:credo]
                             ;;; composer require --dev phpstan/phpstan
                             ;; :php [:phpstan]
                             ;;; composer require --dev friendsoftwig/twigcs
                             :twig [:twigcs]})
    (au [:BufEnter :BufWritePost :InsertLeave] {: group : callback})))

;;; PLUGINS
(tc diagnostic "enable: assign-type-mismatch")
(tc type LazySpec)
(local P {1 :mfussenegger/nvim-lint :event [:BufReadPre :BufNewFile] : config})

;;; UTILITIES
(fn F.twigcs-parser [output _bufnr]
  (when (or (= "" (vim.trim output)) (not output))
    (lua "return {}"))
  (local decoded (vim.json.decode output))
  (when (= 0 decoded.failures)
    (lua "return {}"))
  (local violations (?. decoded :files 1 :violations))
  (when (not violations) (lua "return {}"))
  (icollect [_ violation (ipairs (or violations []))]
    (let [{: line : severity} violation]
      {:lnum (or (and (= :number (type line)) (- line 1)) 0)
       :col 0
       :message violation.message
       :severity (assert (. severity-map severity)
                         (.. "missing mapping for severity " violation.type))
       :source twigcs-bin})))

P
