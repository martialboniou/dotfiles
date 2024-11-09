(import-macros {: tc} :hondana-dev.macros)

(require :hondana-dev/remap)
(require :hondana-dev/set)

(macro imports! [...]
  (icollect [_ e (ipairs [...])]
    {:import e}))

;; prohibit false warnings from `with-open`
;; - `xpcall` invalidates the need of nil checking
;; - `or` variable can be nil (no big deal here)
(lua "---@diagnostic disable: need-check-nil")
(lua "---@diagnostic disable: cast-local-type")

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
(tc type boolean)
(local developer-mode true)
(when (and developer-mode (-> :Windows (not= _G.jit.os)))
  (local fennel-link (-?> :config (vim.fn.stdpath) (.. :/fnl/fennel.lua)))
  (when (and fennel-link (= 0 (vim.fn.filereadable fennel-link)))
    (let [env (require :tangerine.utils.env)
          version (or (env.get :compiler :version) :latest)
          tangerine-path (.. (vim.fn.stdpath :data)
                             :/lazy/tangerine.nvim/lua/?.lua)
          ;; FIX: fennel-ls: unknown-module-field: false
          get-file #(package.searchpath $ tangerine-path)
          make-command #[:ln :-s $ fennel-link]]
      (var file (get-file (.. :tangerine.fennel. version)))
      (var first-line "")
      ;;
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
                (let [target (get-file extract)]
                  (when (and target
                             (with-open [fin (io.open target)]
                               (not= "" ((fin:lines)))))
                    (set file target)))))
            ;;
            (lua "---@diagnostic enable: need-check-nil")
            (lua "---@diagnostic enable: cast-local-type")
            ;; async
            (var handle nil)
            (local {:new_pipe new
                    : spawn
                    :read_start start
                    :read_stop stop
                    : close} vim.uv)
            ;;
            ;; prohibit false warnings from `&` destructuring
            (lua "---@diagnostic disable: redefined-local")
            (let [[cmd & args] (make-command file)
                  pipes [(new) (new)] ;; no input in this stdio
                  stdio [nil (unpack pipes)]
                  on-exit #(do
                             (each [_ p (ipairs pipes)]
                               (stop p)
                               (close p))
                             (close handle)
                             (when (not= 0 $)
                               (print (.. :exited... (tostring $)))))
                  options {: args : stdio}]
              (set handle (spawn cmd options on-exit))
              (each [_ p (ipairs pipes)]
                ;; $2 = data
                (start p #(when $2 (print $2))))))))))
