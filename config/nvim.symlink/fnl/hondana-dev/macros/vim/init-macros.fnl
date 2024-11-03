;; fennel-ls: macro-file
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
               ,(unpack out))))
 :lazy-types (λ []
               {:deprecated :v1.0
                :fnl/docstring "DON'T use; types.lua is incomplete"}
               "create a Lua escape hatch from the lazy types for the Lua language server;
                this should print the annotations at comptime"
               (let [file (-> :data (vim.fn.stdpath)
                              (.. :/lazy/lazy.nvim/lua/lazy/types.lua)
                              (io.open :r))]
                 (when file
                   (let [content# (file:read :*a)]
                     (file:close)
                     `(lua ,content#)))))}
