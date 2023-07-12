return {
    {
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
            'nvim-treesitter/playground'
        },
        build = ":TSUpdate",
        event = { "BufReadPost", "BufNewFile", },
        cmd = { "TSUpdateSync" },
        opts         = {
            ensure_installed = {
                "typescript",
                "tsx",
                "javascript",
                "json",
                "c",
                "lua",
                "vim",
                "vimdoc",
                "rust",
                "markdown",
                "markdown_inline",
                "html",
                "php",
                "yaml",
            },
            sync_install = false,
            auto_install = true,
            rainbow = {
                enable = true,
                extended_mode = true,
            },
            playground = {
                enable = true,
            },
            highlight = {
                enable = true,
                additional_vim_regex_highlighting = false,
            },
        },
        --[[
        build        = function()
        pcall(require('nvim-treesitter.install').update { with_sync = true })
        end,
        --]]
    },
}
