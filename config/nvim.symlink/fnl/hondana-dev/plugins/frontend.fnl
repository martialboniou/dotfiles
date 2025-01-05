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
                      (setup $2))}])

P
