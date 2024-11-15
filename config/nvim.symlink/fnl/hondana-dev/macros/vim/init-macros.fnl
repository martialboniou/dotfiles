;; fennel-ls: macro-file
;; NeoVim related macros
{:set!! (λ [x ...]
          "set the vim.opt options to x"
          (icollect [_ opt (ipairs [...])]
            (assert-compile (= :string (type opt))
                            "expected string for every vararg"))
          (let [out []]
            (each [_ o (ipairs [...])]
              (table.insert out `(set (. vim.opt ,o) ,x)))
            `(do
               ,(unpack out))))
 :tc-source (λ [directory]
              "make a Lua espace hatch for typechecking with lua-language-server by
  printing a `@source` annotation for a Neovim subdirectory"
              (let [path# (.. _G.vim.env.VIMRUNTIME :/lua)]
                (when path#
                  `(tc ,(.. "source file://" path# directory)))))}
