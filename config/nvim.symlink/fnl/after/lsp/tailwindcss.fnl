;; `npm i -g @tailwindcss/language-server` or `:MasonInstall tailwindcss`

;; F = utility functions
(local F {})

(fn F.insert-package-json [root-files field fname]
  (F.root-markers-with-field root-files [:package.json :package.json5] field
                             fname))

(fn F.root-markers-with-field [root-files new-names field fname]
  (local {: fs} vim)
  (local path (vim.fn.fnamemodify fname ":h"))
  (local found (-> new-names (fs.find {: path :upward true}) (or [])))
  (for [i 1 (length found)]
    (local f (. found i))
    (each [line (io.lines f)]
      (when (line:find field)
        (table.insert root-files (fs.basename f))
        (lua :break))))
  root-files)

{:filetypes [;; html
             :aspnetcorerazor
             :astro
             :astro-markdown
             :blade
             :clojure
             :django-html
             :htmldjango
             :edge
             :eelixir
             :elixir
             :ejs
             :erb
             :eruby
             :gohtml
             :gohtmltmpl
             :haml
             :handlebars
             :hbs
             :html
             :htmlangular
             :html-eex
             :heex
             :jade
             :leaf
             :liquid
             :markdown
             :mdx
             :mustache
             :njk
             :nunjucks
             :php
             :razor
             :slim
             :twig
             ;;  css
             :css
             :less
             :postcss
             :sass
             :scss
             :stylus
             :sugarss
             ;; js
             :javascript
             :javascriptreact
             :reason
             :rescript
             :typescript
             :typescriptreact
             ;; mixed
             :vue
             :svelte
             :templ]
 :cmd [:tailwindcss-language-server :--stdio]
 :settings {:tailwindCSS {:validate true
                          :lint {:cssConflict :warning
                                 :invalidApply :error
                                 :invalidScreen :error
                                 :invalidVariant :error
                                 :invalidConfigPath :error
                                 :invalidTailwindDirective :error
                                 :recommendedVariantOrder :warning}
                          :classAttributes [:class
                                            :className
                                            "class:list"
                                            :classList
                                            :ngClass]
                          :includeLanguages {:eelixir :html-eex
                                             :elixir :phoenix-heex
                                             :eruby :erb
                                             :heex :phoenix-heex
                                             :htmlangular :html
                                             :templ :html}}}
 :before_init (fn [_ config]
                (local {: lsp} vim)
                (local ensure-node
                       #(let [node (. $1 $2)]
                          (if node
                              node
                              (do
                                (set (. $1 $2) $3)
                                (. $1 $2)))))
                (-> config (ensure-node :settings {}) (ensure-node :editor {})
                    (ensure-node :tabSize (lsp.util.get_effective_tabstop))))
 :workspace_required true
 :root_dir (fn [bufnr on-dir]
             (local {: api : fs} vim)
             (local root-files
                    [;; generic
                     :tailwind.config.js
                     :tailwind.config.cjs
                     :tailwind.config.mjs
                     :tailwind.config.ts
                     :postcss.config.js
                     :postcss.config.cjs
                     :postcss.config.mjs
                     :postcss.config.ts
                     ;; Phoenix
                     :assets/tailwind.config.js
                     :assets/tailwind.config.cjs
                     :assets/tailwind.config.mjs
                     :assets/tailwind.config.ts
                     ;; Django
                     :theme/static_src/tailwind.config.js
                     :theme/static_src/tailwind.config.cjs
                     :theme/static_src/tailwind.config.mjs
                     :theme/static_src/tailwind.config.ts
                     ;; Rails
                     :app/assets/stylesheets/application.tailwind.css
                     :app/assets/tailwind/application.css])
             (local fname (api.nvim_buf_get_name bufnr))
             (-> root-files (F.insert-package-json :tailwindcss fname)
                 (F.root-markers-with-field [:mix.lock] :tailwind fname)
                 (fs.find {:path fname :upward true}) (. 1) (fs.dirname)
                 (on-dir)))}
