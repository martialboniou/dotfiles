;; NOTE: more reliable than debugloop/telescope-undo.nvim
(lua "---@type LazySpec")
(local undotree ;;
       {1 :mbbill/undotree
        :cmd :UndotreeToggle
        :keys [{1 :<leader>u
                2 vim.cmd.UndotreeToggle
                :desc "Toggle the undo-tree panel"}]})

undotree
