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

local tomorrow_night = {
    'echasnovski/mini.base16',
    priority = 1100,
    lazy = false,
    config = function()
        require("mini.base16").setup({
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

            palette = {
                base00 = '#1d1f21', -- [TOMORROW BACKGROUND] default background
                base01 = '#3a475e', -- [TODO] lighter background (status bar...)
                base02 = '#373b41', -- (*selection; unused in book) selection background
                base03 = '#969896', -- [TOMORROW COMMENT] comments, invisibles
                base04 = '#282a2e', -- (*line; unused in book) dark foreground (status bar...)
                base05 = '#c5c8c6', -- [TOMORROW FOREGROUND] default foreground (caret, delimiters, operators)
                base06 = '#eff69c', -- [TODO] (RARE) light foreground
                base07 = '#fcffaa', -- [TODO] (RARE) light background
                base08 = '#cc6666', -- [TOMORROW RED] variables, XML tags, markup lint text, markup lists, diff deleted
                base09 = '#de935f', -- [TOMORROW ORANGE] numbers, boolean, constants, XML attributes, markup link url
                base0A = '#c5c8c6', -- [TOMORROW FOREGROUND] classes, markup bold, search text background
                base0B = '#b5bd56', -- [TOMORROW GREEN]  strings, inherited class, markup code, diff inserted
                base0C = '#cc6666', -- [TOMORROW RED] support, regexp, escape chars, markup quotes
                base0D = '#81a2be', -- [TOMORROW BLUE] functions, methods, attribute IDs, headings
                base0E = '#b294bb', -- [TOMORROW PURPLE] keywords, storage, selector, markup italic, diff changed
                base0F = '#00a5c5', -- [TODO] deprecated, opening/closing embedded language tags (e.g. <?php )
            },
            use_cterm = true,
            plugins = {
                default = true,
            },
        })
    end,
}

local tokyonight = {
    "folke/tokyonight.nvim",
    priority = 1200,
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
                -- colors.bg_highlight = "#1d1f21"
                -- colors.terminal_black = "#1d1f21"
                colors.fg = "#c5c8c6" -- tomorrow foreground
                -- colors.fg_dark = "#c5c8c6"
                -- colors.fg_gutter = "#c5c8c6"
                -- colors.dark3 = "#545c7e" -- X
                colors.comment = "#565f89" -- tomorrow comment
                -- colors.dark5 = "#737aa2" -- X
                -- colors.blue0 = "#3d59a1"
                -- colors.blue = "#7aa2f7"
                -- colors.cyan = "#7dcfff"
                colors.blue1 = "#8abeb7" -- tomorrow aqua (type identifier)
                -- colors.blue1 = "#c5c8c6" -- tomorrow foreground (identifier)
                -- NOTE: identifier != type identifier
                -- colors.blue2 = "#0db9d7"
                -- colors.blue5 = "#89ddff"
                -- colors.blue6 = "#b4f9f8"
                -- colors.blue7 = "#394b70"
                colors.magenta = "#b294bb" -- tomorrow purple (keyword fn)
                -- colors.magenta2 = "#ff007c"
                colors.purple = "#b294bb"  -- tomorrow purple (keyword let)
                -- colors.orange = "#ff9e64"
                -- colors.yellow = "#e0af68"
                colors.green = "#b5bd68" -- tomorrow green (string)
                -- colors.green1 = "#73daca"
                -- colors.green2 = "#41a6b5"
                -- colors.teal = "#1abc9c"
                -- colors.red = "#f7768e"
                -- colors.red1 = "#db4b4b"

                -- colors.git = { change = "#6183bb" add = "#449dab" delete = "#914c54" }
                -- colors.gitSigns = {
                --   add = "#266d6a"
                --   change = "#536c9e"
                --   delete = "#b2555b"


                -- NOTE: lua vim.api.nvim_set_hl(0, "@lsp.variable.type.namespace.type.type", { link = "Comment"})
            end,
        })
        vim.cmd [[ colorscheme tokyonight ]]
    end,
}

local zephyr = {
    "glepnir/zephyr-nvim",
    priority = 1300,
    lazy = false,
    dependencies = {
        "nvim-treesitter/nvim-treesitter",
    },
    config = function()
        require("zephyr-nvim").setup({

        })
        vim.cmd [[ colorscheme zephyr ]]
    end,
}

local minimal = {
    "Yazeed1s/minimal.nvim",
    priority = 1400,
    lazy = false,
    dependencies = {
        "nvim-treesitter/nvim-treesitter",
    },
    config = function()
        vim.g.minimal_italic_comments = false
        vim.cmd [[ colorscheme minimal-base16 ]]
    end,
}

local enfocado = {
    "wuelnerdotexe/vim-enfocado",
    priority = 1500,
    lazy = false,
    config = function()
        vim.g.enfocado_style = "nature"
        vim.cmd [[ colorscheme enfocado ]]
    end,
}

local lourenci_github_colors = {
    "lourenci/github-colors",
    priority = 1600,
    lazy = false,
    config = function()
        vim.cmd [[ colorscheme github-colors ]]
    end,
}

return {
    -- rose_pine,
    -- tomorrow_night,
    tokyonight, -- type_identifier == identifier in TS (it's wrong)
    -- zephyr,
    --  minimal,
    -- enfocado,
    -- lourenci_github_colors,
}
