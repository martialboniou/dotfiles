return {
    -- treesitter
    {
        'nvim-treesitter/nvim-treesitter',
        opts  = {
            ensure_installed = {
                "typescript",
                "javascript",
                "c",
                "lua",
                "vim",
                "vimdoc",
                "rust",
            },
            sync_install = false,
            auto_install = true,

            highlight = {
                enable = true,
                additional_vim_regex_highlighting = false,
            },
        },
        build = function()
            pcall(require('nvim-treesitter.install').update { with_sync = true })
        end,
    },
    -- playground
    {
        'nvim-treesitter/playground'
    },
}
