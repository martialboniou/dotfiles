(import-macros {: tc} :hondana-dev.macros)

(tc type "fun(self:LazyPlugin, opts:table)")
(local config #(let [{: setup} (require :nvim-ts-autotag)]
                 (setup $2)))

(tc type "fun(self:LazyPlugin, opts:table)")
(local css-colors-config #(let [{: setup} (require :nvim-highlight-colors)]
                            (setup {})))

(local init #(set vim.g.mkdp_filetypes [:markdown]))

(tc type "LazySpec[]")
(local P [;; server-side template engines
          ;  (very optional) highlights in html.twig files
          ;; {1 :nelsyeung/twig.vim :ft [:html.twig.js.css]}
          ;; autotag markup languages: html, xml, jsx, tsx...
          {1 :windwp/nvim-ts-autotag
           :lazy true
           :event :VeryLazy
           ;; TODO: unsure about deps here
           :dependencies :nvim-treesitter/nvim-treesitter
           : config}
          {1 :iamcco/markdown-preview.nvim
           :cmd [:MarkdownPreviewToggle :MarkdownPreview :MarkdownPreviewStop]
           :ft :markdown
           :build "cd app && npx --yes yarn install"
           : init}
          ;; NOTE:new changes: display CSS colors
          {1 :brenoprata10/nvim-highlight-colors
           :event :VeryLazy
           :config css-colors-config}])

P
