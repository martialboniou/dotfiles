return {
    -- telescope (may change back to fzf or lf)
    'nvim-telescope/telescope.nvim',
    tag = '0.1.2',
    dependencies = { { 'nvim-lua/plenary.nvim' } },
    keys = {
        -- files & buffers
        {
            '<leader>pf',
            function()
                require("telescope.builtin").find_files()
            end,
            { desc = "Open a fuzzy file picker", },
        },
        {
            '<leader>pr',
            function()
                require("telescope.builtin").oldfiles()
            end,
            { desc = "Open a fuzzy recent file picker", },
        },
        {
            '<leader>pb',
            function()
                require("telescope.builtin").buffers()
            end,
            { desc = "Open a fuzzy buffer picker", },
        },
        -- rg-filtered project files
        {
            '<leader>ps',
            function()
                require("telescope.builtin").grep_string({
                    search = vim.fn.input("Grep > ")
                });
            end,
            { desc = "Open a string-filtered file picker (cwd)", },
        },
        -- helptags
        {
            '<leader>vh',
            function()
                require("telescope.builtin").help_tags()
            end,
            { desc = "Open a help tag picker", },
        },
        {
            '<leader>ph',
            function()
                require("telescope.builtin").help_tags()
            end,
            { desc = "Open a help tag picker", },
        },
        -- git files
        {
            '<C-p>',
            function()
                require("telescope.builtin").git_files()
            end,
            { desc = "Open a git repo fuzzy file picker", },
        },

    },
}
