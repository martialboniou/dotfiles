(import-macros {: tc} :hondana-dev.macros)
;; core plugins for this NeoVim
(tc type LazySpec)
(local T [;; Fennel Integration
          {1 :udayvir-singh/tangerine.nvim
           :priority 1500
           :lazy false
           :keys [{1 :gC
                   2 :<cmd>FnlCompileBuffer<CR>
                   :desc "Compile into a Lua file"}
                  [:gd :<cmd>FnlCompile<CR>]]
           ;; the setup has already been done from `.config/nvim/init.lua`
           ;; TODO: clean up; make an utility function
           :config #(let [addG-fennel [":command!"
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
                      ;; additional commands:
                      ;;   FnlAddG adds globals to the tangerine.fennel's compiler
                      ;; usage (example to compile love2d.org's code from tangerine)
                      ;;   :FnlAddG love
                      (-> addG-fennel (table.concat " ") (vim.cmd)))}
          ;; Fennel Macros
          :udayvir-singh/hibiscus.nvim])

;; ensure P can match a table type
(tc type "LazySpec[]")
(local P T)

;; Marks: marks.nvim to improve the mark navigation
;; memo:
;;   - m,: set the next available mark
;;   - m;: toggle the next available mark at the current line
;;   - mx: mark x
;;   - dmx: delete mark x
;;   - dm-: delete marks on the current line
;;   - dm<Space>: delete marks in the current buffer
;;   - m : move to next mark
(->> :chentoast/marks.nvim
     (#{1 $ :event :VeryLazy :opts {}})
     (table.insert P))

(let [init #(set vim.g.startuptime_tries 10)]
  (->> :dstein64/vim-startuptime
       (#{1 $ :cmd :StartupTime : init})
       (table.insert P)))

P
