(import-macros {: g!} :hibiscus.vim)

;; for now, conjure is used in cmp (check plugins/completions)
{1 :Olical/conjure
 :ft [:fennel :scheme :rust :clojure]
 :init #(do
          (g! "conjure#mapping#doc_word" :<localleader>K)
          (g! "conjure#debug" false)
          ;; (g! "conjure#filetype#fennel" :conjure.client.fennel.stdio)
          )
 :dependencies [; :Olical/aniseed
                {1 :PaterJason/cmp-conjure :dependencies :hrsh7th/nvim-cmp}]}
