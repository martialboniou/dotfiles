(import-macros {: tc} :hondana-dev.macros)

(macro imports! [...]
  (icollect [_ e (ipairs [...])]
    {:import e}))

;; define your roles in this setup:
;; - `rustacean` supercharges your rust experience
;; - `haskell-cultist` sets `haskell-tools.nvim` instead of `hls`
;; - `developer` says you want to extend this setup by coding in Fennel
;; - `commentator` says you don't enjoy grayish comments as it's difficult to
;; read 
(local {: roles} (require :hondana-dev.utils.globals))
(roles:set [:developer :commentator :haskell-cultist])

;; WARN: this module depends on `roles`
(require :hondana-dev)

;; set the list of your plugin specs' directories here
(local plugins (imports! :hondana-dev.plugins :hondana-dev.plugins.unchecked))

(local {:fn {: stdpath : system : filereadable}
        :fs {: joinpath}
        :uv {: fs_stat}
        :opt {: runtimepath}} vim)

;;; BOOTSTRAP LAZY
;;
(tc type string)
(local lazy-directory (-> :data (stdpath) (joinpath :lazy)))
(let [lazy-path (joinpath lazy-directory :lazy.nvim)]
  (when (not (fs_stat lazy-path))
    (system [:git
             :clone
             "--filter=blob:none"
             "https://github.com/folke/lazy.nvim.git"
             :--single-branch
             lazy-path]))
  (runtimepath:prepend lazy-path))

(let [{: setup} (require :lazy)
      opts {:checker {:enabled false}
            :defaults {:lazy true}
            :ui {:change_detection {:notify false}}
            :performance {:rtp {:reset false
                                :disabled_plugins [:tutor :matchit :matchparen]}}
            :spec plugins}]
  (setup opts))

;;; OPTIONAL (not required but very handy)
;; auto-link fennel.lua from Tangerine to `~/.config/nvim/fnl/fennel.lua`
(local {: posix} (require :hondana-dev.utils.globals))
(when (and (roles:check :developer) posix)
  (tc type string?)
  (local target (-?> :config (stdpath) (joinpath :fnl :fennel.lua)))
  (when (and target (= 0 (filereadable target)))
    (let [{:link-tangerine-fennel-lua link} (require :hondana-dev.utils.boot-helper)]
      (link target lazy-directory))))
