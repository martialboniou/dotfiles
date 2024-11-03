(import-macros {: g!} :hibiscus.vim)

;; for now, conjure is used in cmp (check plugins/completions)
(lua "---@type LazySpec")
(local conjure ;;
       {1 :Olical/conjure
        :ft [:fennel :scheme :rust :clojure]
        :init #(do
                 ;; (g! "conjure#filetype#fennel" :conjure.client.fennel.stdio)
                 (g! "conjure#mapping#doc_word" :<localleader>K)
                 (g! "conjure#debug" false))
        :dependencies [{1 :PaterJason/cmp-conjure
                        :dependencies :hrsh7th/nvim-cmp}]})

conjure
