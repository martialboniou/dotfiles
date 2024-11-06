(require :hondana-dev/remap)
(require :hondana-dev/set)

(macro imports! [...]
  (icollect [_ e (ipairs [...])]
    {:import e}))

;; set the list of your plugin specs' directories here
(local plugins (imports! :hondana-dev.plugins :hondana-dev.plugins.unchecked))

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

;; OPTIONAL (not required but very handy)
;; auto-link fennel.lua from Tangerine to `~/.config/nvim/fnl/fennel.lua`
(local developer-mode true)
(when (and developer-mode (-> :Windows (not= _G.jit.os)))
  (local fennel-link (-?> :config (vim.fn.stdpath) (.. :/fnl/fennel.lua)))
  (when (and fennel-link (= 0 (vim.fn.filereadable fennel-link)))
    (local env (require :tangerine.utils.env))
    (local version (or (env.get :compiler :version) :latest))
    (local tangerine-path
           (.. (vim.fn.stdpath :data) :/lazy/tangerine.nvim/lua/?.lua))
    (local get-file #(package.searchpath $ tangerine-path))
    (local make-command #[:ln :-s $ fennel-link])
    (var file (get-file (.. :tangerine.fennel. version)))
    (var first-line "")
    (if (or (not file) (with-open [fin (io.open file)]
                         (set first-line ((fin:lines)))
                         (= "" first-line)))
        (print "You should have a fennel.lua file in your ~/.config/nvim/fnl directory")
        ;; :else
        (do
          ;; get the source code if possible (to avoid a dependency on the Tangerine's path)
          (when (first-line:match :require)
            (local extract (first-line:gsub ".*require%(\"(.*)\"%).*" "%1"))
            ;; check if the require'd module is in tangerine
            (when (extract:match "^(tangerine)")
              (local target (get-file extract))
              (when (and target
                         (with-open [fin (io.open target)]
                           (not= "" ((fin:lines)))))
                (set file target))))
          ;; async
          (local [cmd & args] (make-command file))
          (local {:new_pipe new
                  : spawn
                  :read_start start
                  :read_stop stop
                  : close
                  : spawn} vim.uv)
          (local pipes [(new) (new)])
          (local stdio [nil (unpack pipes)])
          (var handle nil)
          (local on-exit
                 #(do
                    (each [_ p (ipairs pipes)]
                      (stop p)
                      (close p))
                    (close handle)
                    (when (not= 0 $)
                      (print (.. :exited... (tostring $))))))
          (local options {: args : stdio})
          (set handle (spawn cmd options on-exit))
          (each [_ p (ipairs pipes)]
            ;; $2 = data
            (start p #(when $2 (print $2))))))))
