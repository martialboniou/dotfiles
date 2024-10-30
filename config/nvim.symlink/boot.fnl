(require :hondana-dev/remap)
(require :hondana-dev/set)

(macro imports! [...]
  (icollect [_ e (ipairs [...])]
    {:import e}))

; set the list of your plugin specs' directories here
(local plugins (imports! :hondana-dev.plugins :hondana-dev.plugins.unchecked))

;; fennel self-contained libraries won't compile on luajit non-compatable 5.2
;; - I recommend to compile with the `fennel` script using lua (or luajit with 5.2 compatibility)
;; - I recommend to separate the macro libraries from the modules (and name them
;;   according to the pattern `*-macros.fnl` to avoid the auto-compilation by
;;   `tangerine.nvim`)
;; - I also use a neovim 0.10 based on luajit with 5.2 compatibility;
;;   so I can compile these self-contained libraries from `tangerine.nvim`
;;   without errors (I just need to ensure that the missing `package.searchers`
;;   in the luajit non-compatible 5.2 is here in my dirty build)
(when (-> (vim.version) (. :build) (= :dirty))
  (-> :tangerine.fennel
      (require)
      (#($.load :latest))
      (#($.install))))

;; bootstrap lazy
(let [lazy-path (.. (vim.fn.stdpath :data) :/lazy/lazy.nvim)]
  (when (not (vim.uv.fs_stat lazy-path))
    (vim.fn.system [:git
                    :clone
                    "--filter=blob:none"
                    "https://github.com/folke/lazy.nvim.git"
                    :--single-branch
                    lazy-path]))
  (vim.opt.runtimepath:prepend lazy-path))

(let [{: setup} (require :lazy)
      opts {:checker {:enabled false}
            :defaults {:lazy true}
            :performance {:rtp {:reset false :disabled_plugins [:tutor]}}
            :spec plugins}]
  (setup opts))
