(lua "---@type LazySpec")
(local P [;; server-side template engines
          ;  (very optional) highlights in html.twig files
          ;; {1 :nelsyeung/twig.vim :ft [:html.twig.js.css]}
          ;; autotag markup languages: html, xml, jsx, tsx...
          {1 :windwp/nvim-ts-autotag
           :lazy true
           :event :VeryLazy
           ;; TODO: unsure about deps here
           :dependencies :nvim-treesitter/nvim-treesitter
           :config #(let [{: setup} (require :nvim-ts-autotag)]
                      (setup $2))}
          {1 :iamcco/markdown-preview.nvim
           :cmd [:MarkdownPreviewToggle :MarkdownPreview :MarkdownPreviewStop]
           :ft :markdown
           :build "cd app && npm i"
           :init #(set vim.g.mkdp_filetypes [:markdown])}])

P
