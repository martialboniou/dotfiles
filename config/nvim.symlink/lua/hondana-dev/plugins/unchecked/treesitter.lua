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
                -- "go", -- add me when you go
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
            incremental_selection = { enable = true, },
            indent = { enable = true, },
            textobjects = {
                select = {
                    enable = true,
                    lookahead = true,
                    keymaps = {
                        ["af"] = "@function.outer",
                        ["ac"] = "@class.outer",
                        ["if"] = "@function.inner",
                        ["ic"] = "@class.inner",
                        ["as"] = { query = "@scope", query_group = "locals" },
                    },
                    selection_modes = {
                        ['@parameter.outer'] = "v",
                        ['@function.outer'] = "V",
                        ['@class.outer'] = "<C-v>",
                    },
                },
                move = {
                    enable = true,
                    set_jumps = true,
                    goto_next_start = {
                        ["]m"] = "@function.outer",
                        ["]]"] = "@class.outer",
                    },
                    goto_next_end = {
                        ["]M"] = "@function.outer",
                        ["]["] = "@class.outer",
                    },
                    goto_previous_start = {
                        ["[m"] = "@function.outer",
                        ["[["] = "@class.outer",
                    },
                    goto_previous_end = {
                        ["[M"] = "@function.outer",
                        ["[]"] = "@class.outer",
                    },
                },
            },
        },
    },
}
