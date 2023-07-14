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
            function()
                require("harpoon.mark").add_file()
            end,
            desc = {
                "Harpoon the current file",
            },
        },
        {
            "<C-e>",
            function()
                require("harpoon.ui").toggle_quick_menu()
            end,
            desc = {
                "Toggle the harpoon's quick menu",
            },
        },
        -- dvorak keys: H,T,N,S --> #1,#2,#3,#4
        {
            "<C-h>",
            function()
                require("harpoon.ui").nav_file(1)
            end,
            desc = {
                "Go to the #1 harpooned file",
            },
        },
        {
            "<C-t>",
            function()
                require("harpoon.ui").nav_file(2)
            end,
            desc = {
                "Go to the #2 harpooned file",
            },
        },
        {
            "<C-n>",
            function()
                require("harpoon.ui").nav_file(3)
            end,
            desc = {
                "Go to the #3 harpooned file",
            },
        },
        {
            "<C-s>",
            function()
                require("harpoon.ui").nav_file(4)
            end,
            desc = {
                "Go to the #4 harpooned file",
            },
        },
    },
}
