return {
    'theprimeagen/harpoon',
    lazy = true,
    dependencies = {
        'nvim-lua/plenary.nvim',
    },
    opts = {
        menu = {
            width = vim.api.nvim_win_get_width(0) - 4,
        },
    },
    keys = {
        {
            "<leader>a",
            function() -- NOTE: harpoon files selected in Netrw/mini.files too
                local buf_type = vim.api.nvim_buf_get_option(0, "filetype")
                if (buf_type == "netrw") then
                    vim.cmd [[ let netrw#current_word = netrw#Call('NetrwFile', netrw#Call('NetrwGetWord')) ]]
                    local path = vim.g["netrw#current_word"] -- TODO: get rid of VimL
                    if vim.fn.isdirectory(path) == 0 then
                        require("harpoon.mark").add_file(path)
                        return
                    end
                end
                local buf_name = vim.api.nvim_buf_get_name(0)
                if (buf_type == "minifiles" and buf_name == "" or buf_name == nil) then
                    local ok, res = pcall(require("mini.files").get_target_window)
                    if ok and res ~= nil then
                        local fs_entry = require("mini.files").get_fs_entry()
                        if fs_entry.fs_type == "file" then
                            require("harpoon.mark").add_file(fs_entry.path)
                            return
                        end
                    end
                end
                require("harpoon.mark").add_file()
                -- expected behaviors:
                --     error if the cursor in Netrw is not above a file
                --     error if mini.files selection is not a file
            end,
            desc = "Harpoon the current file",

        },
        {
            "<leader>e", -- it was <C-e> but <C-a>/<C-e> = cursor navi in terms
            function()
                require("harpoon.ui").toggle_quick_menu()
            end,
            desc = "Toggle the harpoon's quick menu",

        },
        -- dvorak keys: H,T,N,S --> #1,#2,#3,#4
        {
            "<C-h>",
            function()
                require("harpoon.ui").nav_file(1)
            end,
            desc = "Go to the #1 harpooned file",

        },
        {
            "<C-t>",
            function()
                require("harpoon.ui").nav_file(2)
            end,
            desc = "Go to the #2 harpooned file",

        },
        {
            "<C-n>",
            function()
                require("harpoon.ui").nav_file(3)
            end,
            desc = "Go to the #3 harpooned file",

        },
        {
            "<C-s>",
            function()
                require("harpoon.ui").nav_file(4)
            end,
            desc = "Go to the #4 harpooned file",

        },
    },
}
