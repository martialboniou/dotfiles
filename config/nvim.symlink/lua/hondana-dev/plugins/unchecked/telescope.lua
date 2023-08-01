return {
    -- telescope
    'nvim-telescope/telescope.nvim',
    tag = '0.1.2',
    dependencies = { { 'nvim-lua/plenary.nvim' } },
    keys = {
        -- project
        ---> files (memo: project files)
        {
            '<leader>pf',
            function()
                require("telescope.builtin").find_files()
            end,
            { desc = "Open a fuzzy file picker", },
        },
        ---> rg-filtered files (memo: project search)
        {
            '<leader>ps',
            function()
                require("telescope.builtin").grep_string({
                    search = vim.fn.input("Grep > ")
                });
            end,
            { desc = "Open a string-filtered file picker (cwd)", },
        },
        -- view
        ---> recent files (memo: view viewed/view visited; vr = BAD ERGO)
        {
            '<leader>vv',
            function()
                require("telescope.builtin").oldfiles()
            end,
            { desc = "Open a fuzzy recent file picker", },
        },
        ---> buffers (memo: view buffers)
        {
            '<leader>vb',
            function()
                require("telescope.builtin").buffers()
            end,
            { desc = "Open a fuzzy buffer picker", },
        },
        ---> helptags (memo: view helptags)
        {
            '<leader>vh',
            function()
                require("telescope.builtin").help_tags()
            end,
            { desc = "Open a help tag picker", },
        },
        -- git
        --> files (memo: "control project")
        {
            '<C-p>',
            function()
                require("telescope.builtin").git_files()
            end,
            { desc = "Open a git repo fuzzy file picker", },
        },

    },
}
