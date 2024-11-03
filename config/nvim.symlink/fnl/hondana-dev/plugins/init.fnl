;; core plugins for this NeoVim
(lua "---@type LazySpec")
(local core [;; Fennel Integration
             {1 :udayvir-singh/tangerine.nvim
              :priority 1500
              :lazy false
              :keys [{1 :gC
                      2 :<cmd>FnlCompileBuffer<CR>
                      :desc "Compile into a Lua file"}
                     [:gd :<cmd>FnlCompile<CR>]]
              ;; the setup has already been done from `.config/nvim/init.lua`
              ;; TODO: clean up; make an utility function
              :config #(let [luaAddG [":command!"
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
                         ;; additional command:
                         ;;   FnlAddG adds globals to the tangerine.fennel's compiler
                         ;; usage (example to compile love2d.org's code from tangerine)
                         ;;   :FnlAddG love
                         (-> luaAddG (table.concat " ") (vim.cmd)))}
             ;; Fennel Macros
             :udayvir-singh/hibiscus.nvim])

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
     (table.insert core))

core
