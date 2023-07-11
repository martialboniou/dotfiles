return {
    -- telescope (may change back to fzf or lf)
    'nvim-telescope/telescope.nvim',
    tag = '0.1.2',
    dependencies = { { 'nvim-lua/plenary.nvim' } },
    config = function()
        local builtin = require('telescope.builtin')

        -- files & buffers
        vim.keymap.set('n', '<leader>pf', builtin.find_files, {
            desc = "Open a fuzzy file picker",
        })
        vim.keymap.set('n', '<leader>pr', builtin.oldfiles, {
            desc = "Open a fuzzy recent file picker",
        })
        vim.keymap.set('n', '<leader>pb', builtin.buffers, {
            desc = "Open a fuzzy buffer picker",
        })

        -- rg-filtered project files
        vim.keymap.set('n', '<leader>ps',
            function()
	            builtin.grep_string({
                    search = vim.fn.input("Grep > ")
                });
            end, {
            desc = "Open a string-filtered file picker (cwd)",
        })

        -- helptags
        vim.keymap.set('n', '<leader>vh', builtin.help_tags, {
            desc = "Open a help tag picker",
        })
        vim.keymap.set('n', '<leader>ph', builtin.help_tags, {
            desc = "Open a help tag picker",
        })

        -- git files
        vim.keymap.set('n', '<C-p>', builtin.git_files, {
            desc = "Open a git repo fuzzy file picker",
        })
    end
}
