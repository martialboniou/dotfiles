return {
    {
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
            'nvim-treesitter/playground'
        },
        build        = ":TSUpdate",
        event        = { "BufReadPost", "BufNewFile", },
        cmd          = { "TSUpdateSync" },
        config       = function(_, opts)
            require("nvim-treesitter.configs").setup(opts)
        end,
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
            auto_install = false,
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
    },
}
