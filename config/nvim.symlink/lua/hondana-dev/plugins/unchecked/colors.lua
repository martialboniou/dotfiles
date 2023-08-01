local rose_pine = {
    'rose-pine/neovim',
    priority = 1000,
    lazy = false,
    config = function()
        require("rose-pine").setup({
            disable_background = true,
            disable_float_background = true,
        })
        vim.cmd [[ colorscheme rose-pine-moon ]]
    end,
}

local rainbow_delimiters = {
    "HiPhish/rainbow-delimiters.nvim",
    event = { "BufReadPost", "BufNewFile", },
    dependencies = {
        "nvim-treesitter/nvim-treesitter",
    },
    config = function()
        local rainbow = require("rainbow-delimiters")
        vim.g.rainbow_delimiters = {
            strategy = {
                [""] = rainbow.strategy["global"],
                commonlisp = rainbow.strategy["local"],
            },
            query = {
                [""] = "rainbow-delimiters",
                latex = "rainbow-blocks",
            },
            highlight = { -- shuffle
                "RainbowDelimiterGreen",
                "RainbowDelimiterOrange",
                "RainbowDelimiterCyan",
                "RainbowDelimiterYellow",
                "RainbowDelimiterBlue",
                "RainbowDelimiterRed",
                "RainbowDelimiterViolet",
            },
            blacklist = { "c", "cpp", "h", "hpp", "m", },
        }
    end,
}

local tokyonight = {
    "folke/tokyonight.nvim",
    priority = 1000,
    lazy = false,
    config = function()
        require("tokyonight").setup({
            style = "night",
            styles = {
                comments = {},
                keywords = {},
            },
            -- Tomorrow Night palette based on:
            -- - the https://doc.rust-lang.org/book css theme
            -- - adapted from https://github.com/jmblog/color-themes-for-highlightjs
            -- - originally created by https://github.com/chriskempson/tomorrow-theme
            -- #c5c8c6 -- tomorrow foreground
            -- #1d1f21 -- tomorrow background
            -- *selection: #373b41 (unused in book)
            -- *line: #282a2e (unused in book)
            -- *window: #4d5057 (unused in book)
            -- #969896 -- tomorrow comment (comment)
            -- #cc6666 -- tomorrow red (variable, attribute, tag, regexp, ruby-constant, xml-tag-title, xml-doctype, xml-pi, html-doctype, css-id, css-class, css-pseudo)
            -- #de935f -- tomorrow orange (params, constant, number, preprocessor, pragma, built-in, literal)
            -- #f0c674 -- tomorrow yellow (ruby-class-title, css-rule-attribute)
            -- #b5bd68 -- tomorrow green (string, value, inheritance, header, name, ruby-symbol, xml-cdata)
            -- #8abeb7 -- tomorrow aqua (title, css-hexcolor)
            -- #81a2be -- tomorrow blue (python-decoration+title, ruby-function-title+title-keyword)
            -- #b294bb -- tomorrow purple (hljs-keyword, hljs-function)
            -- #718c00 -- addition (only used in book)
            -- #c82829 -- deletion (only used in book)
            -- WORK IN PROGRESS
            on_colors = function(colors)
                colors.bg_dark = "#1d1f21" -- tomorrow background
                colors.bg = "#1d1f21"      -- tomorrow background
                colors.fg = "#c5c8c6"      -- tomorrow foreground
                colors.comment = "#565f89" -- tomorrow comment

                colors.blue1 = "#c5c8c6"   -- tomorrow foreground (identifier)
                colors.blue6 = "#8abeb7"   -- tomorrow aqua (lsp regexp)
                colors.magenta = "#b294bb" -- tomorrow purple (keyword fn)
                colors.purple = "#b294bb"  -- tomorrow purple (keyword let)

                colors.green = "#b5bd68"   -- tomorrow green (string)

                colors.orange = "#de935f"
                colors.yellow = "#f0c674"

                colors.red = "#cc6666"
            end,
            on_highlights = function(highlights, colors)
                highlights.rainbowcol6 = {
                    fg = colors.magenta2 -- b/c magenta = purple now
                }
            end,
        })
        vim.cmd [[ colorscheme tokyonight ]]
        -- blue6 AKA tomorrow aqua
        vim.api.nvim_set_hl(0, "@lsp.typemod.function.declaration.rust", { link = "@string.regex" })
        vim.api.nvim_set_hl(0, "@lsp.typemod.enum.declaration.rust", { link = "@string.regex" })
        -- Normal
        vim.api.nvim_set_hl(0, "@lsp.type.function.rust", { link = "Normal" })
    end,
}

local kat = {
    "katawful/kat.nvim",
    version = "3.1",
    priority = 1000,
    lazy = false,
    config = function(_, _)
        vim.cmd [[ colorscheme kat.nvim ]]
    end
}

return {
    rose_pine,
    -- tokyonight,
    -- kat,
    rainbow_delimiters,
}
