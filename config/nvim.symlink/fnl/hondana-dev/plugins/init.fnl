(import-macros {: tc} :hondana-dev.macros)

(local in table.insert)
(local api vim.api)

;; additional command for the Tangerine's lazy config function:
;;   FnlAddG adds globals to the tangerine.fennel's compiler
;; usage (example to compile love2d.org's code from tangerine):
;;   :FnlAddG love
(tc type "fun(self:LazyPlugin, opts:table)")
(fn config []
  (let [addG-fennel [":command!"
                     :-nargs=*
                     :FnlAddG
                     :Fnl
                     "("
                     "#(->"
                     "(require :tangerine.utils.env)"
                     "(. :get)"
                     "(#($ :compiler :globals))"
                     "(table.insert $))"
                     :<q-args>
                     ")"]]
    (-> addG-fennel (table.concat " ") (vim.cmd))))

;;; * Core plugins for this NeoVim *

;; ensure P can match a table type
(tc type "LazySpec[]")
(local P [;; Fennel Integration
          {1 :udayvir-singh/tangerine.nvim
           :priority 1500
           :lazy false
           :keys [{1 :gC
                   2 :<cmd>FnlCompileBuffer<CR>
                   :desc "Compile into a Lua file"}]
           ;; the setup has already been done from `.config/nvim/init.lua`
           : config}
          ;; Fennel Macros
          :udayvir-singh/hibiscus.nvim])

;;; * Uncategorized plugins *

;; Sleuth.vim: auto-adjust `tabstop`/`shiftwidth` & `expandtab` based on the current file
;; usage:
;;   :Sleuth
(in P {1 :tpope/vim-sleuth :cmd [:Sleuth]})

;; Marks: marks.nvim to improve the mark navigation
;; memo:
;;   - m,  : set the next available mark
;;   - m;  : toggle the next available mark at the current line
;;   - mx  : mark x
;;   - dmx : delete mark x
;;   - dm- : delete marks on the current line
;;   - dm<Space> : delete marks in the current buffer
;;   - m   : move to next mark
(->> :chentoast/marks.nvim
     (#{1 $ :event :VeryLazy :opts {}})
     (in P))

;; StartupTime: benchmarks startup event timing using the command `:StartupTime`
;; memo:
;;   - K  : get additional information of an event
;;   - gf : load the sourcing event's corresponding file in a split window
;;   - :help startuptime-configuration : for customization
(let [init #(set vim.g.startuptime_tries 10)]
  (->> :dstein64/vim-startuptime
       (#{1 $ :cmd :StartupTime : init})
       (in P)))

;; Persistence: simple session management
(let [event :BufReadPre
      opts {}
      config #(let [{: setup} (require :persistence)]
                (setup $2)
                (api.nvim_create_autocmd :User
                                         {:group (api.nvim_create_augroup :Hondana_Persistence_PostLoadInfo
                                                                          {:clear true})
                                          :pattern :PersistenceLoadPost
                                          :callback #(vim.notify "Session restored!"
                                                                 vim.log.levels.INFO)}))
      keys #(let [{: load : select : stop} (require :persistence)]
              [{1 :<leader>qq
                2 load
                :desc "Load the session for the current directory"}
               {1 :<leader>qs 2 select :desc "Select a session to load"}
               {1 :<leader>ql
                2 #(load {:last true})
                :desc "Load the last session"}
               {1 :<leader>qd
                2 stop
                :desc "Stop Persistence => session won't be saved on exit"}])]
  (->> :folke/persistence.nvim
       (#{1 $ : event : keys : opts : config})
       (in P)))

;; Flash: navigate your code with search labels, enhanced character motions and Treesitter integration
;; usage: `/` or `?` to jump
;;        you can also use `<localleader>s` for a Treesitter motion
;;        (NOTE: `s` is available in visual and operator-pending modes only)
;; TODO: test & setup flash.nvim
(in P {1 :folke/flash.nvim
       :event :VeryLazy
       :opts {;; dvorak friendly keybindings
              :labels "aoeui;qjkxhtnsgcrldixbmwvzpy"
              ;; `/`/`?` is set to flash jump by default
              :modes {:search {:enabled true}}}
       :keys [;; this function is bound to `S` in the recommended install
              {1 :s
               2 #(let [{: treesitter} (require :flash)] (treesitter))
               :mode [:x :o]
               :desc "Flash Treesitter"}
              {;; in Fennel, normal `s`/`S` = `:cal PareditEraseFwd()`/`:cal PareditEraseFwd(visualmode(), 1)`
               1 :<localleader>s
               2 #(let [{: treesitter} (require :flash)] (treesitter))
               :mode [:n :x :o]
               :desc "Flash Treesitter"}
              {1 :r
               2 #(let [{: remote} (require :flash)] (remote))
               :mode [:o]
               :desc "Remote Flash"}
              {1 :R
               2 #(let [{:treesitter_flash tf} (require :flash)] (tf))
               :mode [:o :x]
               :desc "Treesitter Flash"}
              {1 :<C-s>
               2 #(let [{: toggle} (require :flash)] (toggle))
               :mode [:c]
               :desc "Toggle Flash Search"}]})

;; Lspsaga: improve neovim lsp experience (using `winbar` as decorators; this was
;; linked to the `nvim-lspconfig` setup in a previous setup)
(in P {1 :nvimdev/lspsaga.nvim
       :event :LspAttach
       :dependencies [:nvim-treesitter/nvim-treesitter
                      :nvim-tree/nvim-web-devicons]
       :opts {:code_action {:show_server_name true :extend_gitsigns false}
              :lightbulb {:enable false}
              :diagnostic {:on_insert false :on_insert_follow false}
              :rename {:in_select false}}})

;; illuminate.vim: automatic highlighting other uses of the word under the cursor (using LSP & treesitter)
;; (in P {1 :RRethy/vim-illuminate
;;        :event :VeryLazy
;;        :cmd [:IlluminatePause :IlluminateToggle]
;;        :opts {:under_cursor true
;;               :delay 500
;;               :large_file_cutoff 2000
;;               :large_file_overrides {:providers [:lsp]}}
;;        :config (fn [_ opts]
;;                  (let [i (require :illuminate)] (i.configure opts)))})

;; mini.cursorword: automatic highlighting of word under cursor (replace RRethy/vim-illuminate)
(in P {1 :echasnovski/mini.cursorword
       :version false
       :event :VeryLazy
       :opts {:delay 400}
       :config #(let [mc (require :mini.cursorword)] (mc.setup $2))})

;; WhichKey: displays available keybindings in a popup as you type
;; FIXME: no highlighted selection line when `<S-V>` (type `V` before to reenable it)
;; (->> :folke/which-key.nvim
;;      (#{1 $
;;         :event :VeryLazy
;;         :config #(vim.schedule_wrap (let [{: add : setup} (require :which-key)]
;;                                       (setup)
;;                                       ;; TODO: customize (REMINDER: `:hidden true` to disable)
;;                                       ;; HACK: shouldn't be here
;;                                       (add {1 :<leader>I :desc "Paredit raise"})
;;                                       (add {1 :<leader>J :desc "Paredit join"})
;;                                       (add {1 :<leader>O :desc "Paredit split"})
;;                                       (add {1 :<leader>S
;;                                             :desc "Paredit splice"})
;;                                       (add {1 :<leader>W :desc "Paredit wrap"})
;;                                       (add {1 "<leader>("
;;                                             :desc "Paredit toggle"})
;;                                       (add {1 :<leader><
;;                                             :desc "Paredit move left"})
;;                                       (add {1 :<leader>>
;;                                             :desc "Paredit move right"})
;;                                       (add {1 :<leader><Up>
;;                                             :desc "Paredit killing backward splice"})
;;                                       (add {1 :<leader><Down>
;;                                             :desc "Paredit killing forward splice"})))})
;;      (in P))

;; Luvit-Meta: collects meta type definitions for Luvit
(in P {1 :Bilal2453/luvit-meta :lazy true})

P
