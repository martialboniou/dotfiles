;; NeoVim related macros
{:set!! (λ [x ...]
          "set the vim.opt options to x"
          (icollect [_ opt (ipairs [...])]
            (assert-compile (= :string (type opt))
                            "expected string for every vararg"))
          (let [out []]
            (each [_ o (ipairs [...])]
              (table.insert out `(tset vim.opt ,o ,x)))
            `(do
               ,(unpack out))))}
