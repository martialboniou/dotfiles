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
 :setlocal!! (λ [x ...]
               "set the vim.opt_local options to x"
               (icollect [_ opt (ipairs [...])]
                 (assert-compile (= :string (type opt))
                                 "expected string for every vararg"))
               (let [out []]
                 (each [_ o (ipairs [...])]
                   (table.insert out `(set (. vim.opt_local ,o) ,x)))
                 `(do
                    ,(unpack out))))
 :make-lazykeys! (λ [keymaps]
                   "build a keymap table as LazyKeys; accepts a sequence of keys (thus a same function
can share multiple keybindings)"
                   (let [o []
                         table? #(-> $ (type) (= :table))]
                     (each [_ keymap (ipairs keymaps)]
                       (assert-compile (and (sequence? keymap)
                                            (= 3 (length keymap)))
                                       "each element must be a table of 3"
                                       keymap)
                       (var keys (. keymap 1))
                       (when (= :string (type keys)) (set keys [keys]))
                       (assert-compile (and (table? keys) (<= 1 (length keys)))
                                       "each key must be in a sequence" keys)
                       (local tally (length keys))
                       (for [i 1 tally]
                         (var key (. keys i))
                         ;; append <leader> when required
                         (when (not (key:match "^<"))
                           (set key (.. :<leader> key)))
                         (let [fun (. keymap 2)
                               opts (. keymap 3)
                               lazykey (if (table? opts) opts {:desc opts})]
                           (set (. lazykey 1) key)
                           (set (. lazykey 2) fun)
                           (table.insert o lazykey))))
                     o))
 :tc-source (λ [directory]
              "make a Lua espace hatch for typechecking with lua-language-server by
  printing a `@source` annotation for a Neovim subdirectory"
              (let [path# (.. _G.vim.env.VIMRUNTIME :/lua)]
                (when path#
                  `(tc ,(.. "source file://" path# directory)))))}
