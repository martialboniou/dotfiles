(import-macros {: g!} :hibiscus.vim)

{1 :Olical/conjure
    :ft [:fennel :lisp :scheme :lua :rust :clojure :python]
    :init #(g! "conjure#mapping#doc_word" :<localleader>K)
    :dependencies [:Olical/aniseed]}
