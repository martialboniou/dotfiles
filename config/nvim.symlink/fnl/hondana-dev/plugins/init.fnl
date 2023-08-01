;; core plugins for this NeoVim
(local core [;; Fennel Integration
             {1 :udayvir-singh/tangerine.nvim
              :priority 1500
              :lazy false
              :keys [{1 :gC
                      2 :<cmd>FnlCompileBuffer<CR>
                      :desc "Compile into a Lua file"}
                     {1 :gD
                      2 :<cmd>FnlCompile<CR>}]}
             ;; Fennel Macros
             :udayvir-singh/hibiscus.nvim])

core
