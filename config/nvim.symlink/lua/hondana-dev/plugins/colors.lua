local rose_pine = {
    'rose-pine/neovim',
    priority = 1000,
    lazy = false,
    config = function()
        require("rose-pine").setup({
            disable_background = true,
            disable_float_background = true,
        })
        vim.cmd([[colorscheme rose-pine-moon]])
    end,
}

return {
    rose_pine,
}
