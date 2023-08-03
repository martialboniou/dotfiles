return {
    'tpope/vim-fugitive',
    cmd = 'Git',
    keys = {
        { '<leader>gs', vim.cmd.Git, desc = 'Open fuGITive status' },
    },
    config = function()
        local ThePrimeagen_Fugitive = vim.api.nvim_create_augroup("ThePrimeagen_Fugitive", {})
        local autocmd = vim.api.nvim_create_autocmd

        autocmd("BufWinEnter", {
            group = ThePrimeagen_Fugitive,
            pattern = "*",
            callback = function()
                if vim.bo.ft ~= "fugitive" then
                    return
                end

                local bufnr = vim.api.nvim_get_current_buf()
                local options = { buffer = bufnr, remap = false }
                vim.keymap.set("n", "<leader>p", function()
                    vim.cmd.Git('push')
                end, options)

                -- rebase always
                vim.keymap.set("n", "<leader>P", function()
                    vim.cmd.Git({ 'pull', '--rebase' })
                end, options)

                -- NOTE: It allows me to easily set the branch i am pushing and any tracking
                -- needed if i did not set the branch up correctly
                vim.keymap.set("n", "<leader>t", ":Git push -u origin ", options);
            end,
        })
    end,
}
