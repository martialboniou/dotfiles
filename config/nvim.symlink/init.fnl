(require :hondana-dev/remap)
(require :hondana-dev/set)

(macro imports! [...]
  (icollect [_ e (ipairs [...])]
    {:import e}))

; set the list of your plugin specs' directories here
(local plugins (imports! :hondana-dev.plugins :hondana-dev.plugins.unchecked))

;; bootstrap lazy
(let [lazy-path (.. (vim.fn.stdpath :data) :/lazy/lazy.nvim)]
  (when (not (vim.loop.fs_stat lazy-path))
    (vim.fn.system [:git
                    :clone
                    "--filter=blob:none"
                    "https://github.com/folke/lazy.nvim.git"
                    :--single-branch
                    lazy-path]))
  (vim.opt.runtimepath:prepend lazy-path))

(let [opts {:checker {:enabled false}
            :defaults {:lazy true}
            :performance {:rtp {:reset false :disabled_plugins [:tutor]}}
            :spec plugins}]
  (#($.setup opts) (require :lazy)))
