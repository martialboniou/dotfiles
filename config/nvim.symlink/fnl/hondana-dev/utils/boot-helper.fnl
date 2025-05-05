(import-macros {: tc} :hondana-dev.macros)
(local M {})

(tc param target string param _3fsrc_directory? string)
(fn M.link-tangerine-fennel-lua [target ?src-directory]
  "auto-link fennel.lua from Tangerine to `target`; the optional `src-directory` is
  the root of your package manager local plugins (say, `/lazy` at the `data` stdpath)"
  ;(local tangerine-root (or ?src-directory (-> :lazy (vim.fn.stdpath) (.. :/lazy))))
  (let [env (require :tangerine.utils.env)
        version (or (env.get :compiler :version) :latest)
        root (if ?src-directory ?src-directory
                 (let [c (require :lazy.core.config)]
                   c.options.root))
        tangerine-path (.. root :/tangerine.nvim/lua/?.lua)
        ;; FIX: fennel-ls: unknown-module-field: false
        get-file #(package.searchpath $ tangerine-path)
        make-command #[:ln :-s $ target]]
    (var file (get-file (.. :tangerine.fennel. version)))
    (var first-line "")
    ;;
    ;; prohibit false warnings from `with-open`
    ;; - `xpcall` invalidates the need of nil checking
    ;; - `or` variable can be nil (no big deal here)
    (tc diagnostic "disable: need-check-nil")
    (tc diagnostic "disable: cast-local-type")
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
          ;; NOTE: not required but cleaner
          (lua "---@diagnostic enable: need-check-nil")
          (lua "---@diagnostic enable: cast-local-type")
          ;; async
          (var handle nil)
          (tc cast handle uv_handle_t)
          (local {:new_pipe new
                  : spawn
                  :read_start start
                  :read_stop stop
                  : close} vim.uv)
          ;;
          ;; prohibit false warnings from `&` destructuring
          (lua "---@diagnostic disable: redefined-local")
          ;;
          (tc diagnostic disable)
          (local [cmd & args] (make-command file))
          (tc diagnostic enable)
          (let [stdio [nil (new) (new)]
                on-exit #(do
                           (for [i 2 3]
                             (let [p (. stdio i)]
                               (when p
                                 (stop p)
                                 (close p))))
                           (close handle)
                           (when (not= 0 $)
                             (print (.. :exited... (tostring $)))))
                options {: args : stdio}]
            (set handle (spawn cmd options on-exit))
            (for [i 2 3]
              (let [p (. stdio i)]
                ;; $2 = data
                (when p
                  (start p #(when $2 (print $2)))))))))))

(tc type boolean)
(var tangerine-wrapper-done false)
(fn M.tangerine-new-create-float []
  "wraps around the original tangerine.utils.window.create-float once to attach
  an additional keymap to the floating window buffer"
  (when tangerine-wrapper-done (lua :return))
  (let [(has-window window) (pcall require :tangerine.utils.window)]
    (if has-window
        (do
          (tc type function)
          (let [original-create-float window.create-float]
            ;; around wrapper
            (set window.create-float
                 (fn [lineheight filetype hl-normal ?hl-border]
                   ;; fn instead of λ or double assertions
                   (let [buffer (original-create-float lineheight filetype
                                                       hl-normal ?hl-border)]
                     ;; additional keymap
                     (vim.api.nvim_buf_set_keymap buffer :n :<C-c>
                                                  :<Cmd>FnlWinKill<CR>
                                                  {:silent true :noremap true})
                     buffer)))
            (set tangerine-wrapper-done true)))
        ;; :else
        (vim.fn.notify "hondana-dev.utils.boot-helper: No changes expected for tangerine.utils.window.create-float is considered to be a minor issue."))))

M
