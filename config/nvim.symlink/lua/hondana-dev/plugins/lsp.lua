-- table structure by: https://github.com/MuhametSmaili/nvim/blob/main/lua/smaili/plugins/lsp/init.lua
-- LSP Zero version
-- 2023-07-23
return {
    { -- LSP
        "neovim/nvim-lspconfig",
        event = "BufReadPost",
        dependencies = {
            {
                "VonHeikemen/lsp-zero.nvim", -- LSP Zero
                branch = "v2.x",
                lazy = true,
            },
            -- "simrat39/rust-tools.nvim",
            {
                "williamboman/mason.nvim",
                opts = {},
                cmd = "Mason",
                run = ":MasonUpdate",
            },
            {
                "williamboman/mason-lspconfig.nvim", -- lsp conf for mason lsp
                opts = function(_, opts)
                    if type(opts.ensure_installed) == "table" then
                        vim.list_extend(
                            opts.ensure_installed,
                            {
                                "rust_analyzer",
                                -- "codelldb",
                                -- "taplo",
                            })
                    end
                end,
            },
            "hrsh7th/nvim-cmp",         -- see Autocompletion
            "rrethy/vim-illuminate",    -- optional/highlight same word
            {
                "glepnir/lspsaga.nvim", -- optional/fancy navbar
                dependencies = {
                    "nvim-tree/nvim-web-devicons",
                    "nvim-treesitter/nvim-treesitter",
                },
                opts = {
                    code_action = {
                        show_server_name = true,
                        extend_gitsigns = false,
                    },
                    lightbulb = {
                        enable = false,
                    },
                    diagnostic = {
                        on_insert = false,
                        on_insert_follow = false,
                    },
                    rename = {
                        in_select = false,
                    },
                },
            },
        },
        opts = {
            diagnostics = {
                update_in_insert = false,
                virtual_text = true,
            },
            autoformat = true,
            zero_setup = {
                preset = "recommended",
                preferences = {
                    suggest_lsp_servers = false,
                },
                servers = {
                    "tsserver",
                    "rust_analyzer", -- IMPORTANT: only for cargo project
                    -- "eslint",
                    "html",
                    "lua_ls",
                    "jsonls",
                    "tailwindcss",
                    "dockerls",
                    "docker_compose_language_service",
                    "astro",
                    "vimls",
                    "cssls",
                    -- "gopls", -- add me when you go
                },
                sign_icons = {
                    error = "✘",
                    warn = "▲",
                    hint = "⚑",
                    info = "»",
                },
                sign_chars = {
                    error = "E",
                    warn = "W",
                    hint = "H",
                    info = "I",
                },
            },
        },
        config = function(_, opts)
            -- reduce boilerplate code with LSP Zero
            local lsp_zero = require("lsp-zero")
            local lsp_zero_setup = opts.zero_setup

            lsp_zero.nvim_workspace()

            lsp_zero.preset(lsp_zero_setup.preset)
            lsp_zero.ensure_installed(lsp_zero_setup.servers)
            lsp_zero.set_preferences(lsp_zero_setup.preferences)
            lsp_zero.set_sign_icons(lsp_zero_setup.sign_icons) -- or .sign_chars

            vim.diagnostic.config(opts.diagnostics)

            lsp_zero.on_attach(function(_, bufnr)
                local options = { buffer = bufnr, remap = false }

                vim.keymap.set("n", "<leader>f", function()
                    vim.lsp.buf.format()
                end, options)
                vim.keymap.set("n", "gd", function()
                    vim.lsp.buf.definition()
                end, options)
                vim.keymap.set("n", "K", function()
                    vim.lsp.buf.hover()
                end, options)
                vim.keymap.set("n", "<leader>vws", function()
                    vim.lsp.buf.workspace_symbol()
                end, options)
                vim.keymap.set("n", "<leader>vd", function()
                    vim.diagnostic.open_float()
                end, options)
                vim.keymap.set("n", "[d", function()
                    vim.diagnostic.goto_next()
                end, options)
                vim.keymap.set("n", "]d", function()
                    vim.diagnostic.goto_prev()
                end, options)
                vim.keymap.set("n", "<leader>vca", function()
                    vim.lsp.buf.code_action()
                end, options)
                vim.keymap.set("n", "<leader>vrr", function()
                    vim.lsp.buf.references()
                end, options)
                vim.keymap.set("n", "<leader>vrn", function()
                    vim.lsp.buf.rename()
                end, options)
                vim.keymap.set("i", "<C-h>", function()
                    vim.lsp.buf.signature_help()
                end, options)
                -- ! for ergonomics : <leader> + ca = vca, rr = vrr, nn = vrn
                vim.keymap.set("n", "<leader>ca", function()
                    vim.lsp.buf.code_action()
                end, options)
                vim.keymap.set("n", "<leader>rr", function()
                    vim.lsp.buf.references()
                end, options)
                vim.keymap.set("n", "<leader>nn", function()
                    vim.lsp.buf.rename()
                end, options)
            end)

            require("lspconfig").lua_ls.setup(lsp_zero.nvim_lua_ls())
            -- require("lspconfig").rust_analyzer.setup({
            --     on_attach = lsp_zero.on_attach,
            --     settings = {
            --         ["rust_analyzer"] = {
            --             cargo = {
            --                 autoreload = true,
            --                 buildScripts = { enable = true },
            --             },
            --         },
            --     },
            -- })

            lsp_zero.setup()
        end,
    },
    { -- Autocompletion
        "hrsh7th/nvim-cmp",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            {
                "L3MON4D3/LuaSnip",
                build = "make install_jsregexp",
                dependencies = {
                    "rafamadriz/friendly-snippets", -- optional
                },
                config = function()
                    require("luasnip.loaders.from_vscode").lazy_load({
                        paths = vim.fn.stdpath "config" .. "/snippets/vscode"
                    })
                end,
            },
            "saadparwaiz1/cmp_luasnip",
            "hrsh7th/cmp-buffer",    -- optional/buffer words
            "hrsh7th/cmp-path",      -- optional/filesystem paths
            "petertriho/cmp-git",    -- optional/git
            {
                "hrsh7th/cmp-emoji", -- very optional (see sources.emoji)
                -- dependencies = { "nvim-treesitter/nvim-treesitter" },
            },
            "ray-x/lsp_signature.nvim",
            -- "hrsh7th/cmp-nvim-lsp-signature-help" -- replaced by previous
        },
        opts = function()
            -- nvim-lspconfig ensures the lazy loading of LSP Zero
            require("lsp-zero.cmp").extend()

            local cmp = require("cmp")
            -- local cmp_action_from_lsp_zero = require('lsp-zero.cmp').action()

            -- vim.opt.runtimepath:append("~/github/lsp_signature.nvim")

            cmp.setup.filetype(
                { "markdown" }, {
                    sources = cmp.config.sources({
                        { name = "nvim_lsp_signature_help" },
                        { name = "nvim_lsp" },
                        { name = "luasnip" },
                        {
                            name = "buffer",
                            keyword_length = 4,
                        },
                        { name = "path" },
                        {
                            name = "emoji",
                            insert = true,
                        },
                    }),
                })

            cmp.setup.filetype(
                { "gitcommit" }, {
                    sources = cmp.config.sources({
                        { name = "nvim_lsp_signature_help" },
                        { name = "nvim_lsp" },
                        { name = "luasnip" },
                        {
                            name = "buffer",
                            keyword_length = 4,
                        },
                        { name = "path" },
                        { name = "git" },
                        {
                            name = "emoji",
                            insert = true,
                        },
                    }),
                })

            return {
                completion = {
                    completeopt = "menu,menuone,noinsert",
                },
                window = {
                    completion = cmp.config.window.bordered(),
                    documentation = cmp.config.window.bordered(),
                },
                snippet = {
                    expand = function(args)
                        require("luasnip").lsp_expand(args.body)
                    end,
                },
                sources = cmp.config.sources({
                    { name = "nvim_lsp_signature_help" },
                    { name = "nvim_lsp" },
                    { name = "luasnip" },
                    {
                        name = "buffer",
                        keyword_length = 4,
                    },
                    { name = "path" },
                    {
                        name = "emoji",
                        -- emojis are available in strings/comments only
                        -- but some special filetypes: gitcommit/markdown
                        insert = true,
                        entry_filter = function(_, _)
                            local context = require("cmp.config.context")

                            return context.in_treesitter_capture("comment")
                                or context.in_treesitter_capture("string")
                                or context.in_syntax_group("Comment")
                                or context.in_syntax_group("String")
                        end,
                    },
                }),
                -- experimental = { ghost_text = true },
                mapping = require("lsp-zero").defaults.cmp_mappings({
                    ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
                    ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
                    ["<C-Space>"] = cmp.mapping.complete({}),
                    ["<C-y>"] = cmp.mapping.confirm({ select = true }),
                    -- these two lines are already added by LSP Zero
                    -- ["<C-f>"] = cmp_action_from_lsp_zero.luasnip_jump_forward(),
                    -- ["<C-b>"] = cmp_action_from_lsp_zero.luasnip_jump_backward(),
                    -- NOTE: no <C-b> as tmux prefix
                    --       I chose <M-a> as <C-a> is BOF in any terms
                    -- other keys: <C-u><C-d> = scroll; <C-e> = cancel
                    ["<Tab>"] = nil,
                    ["<S-Tab>"] = nil,
                }),
            }
        end,
    },
    { -- Additional Formatters, Diagnostic tools and Spellchecking
        "jay-babu/mason-null-ls.nvim",
        event = { "BufReadPost", "BufNewFile" },
        dependencies = {
            "williamboman/mason.nvim",
            {
                "jose-elias-alvarez/null-ls.nvim",
                dependencies = {
                    "nvim-lua/plenary.nvim",
                },
                opts = function()
                    local nls = require("null-ls")
                    local h = require("null-ls.helpers")
                    -- formatter for Twig/Nunjucks template
                    local twig_formatter = h.make_builtin {
                        name = "twig-formatter",
                        method = nls.methods.FORMATTING,
                        filetypes = { "html.twig.js.css", },
                        generator_opts = {
                            command = "djlint",
                            args = {
                                "--no-function-formatting", -- MANDATORY
                                "--profile=nunjucks",
                                "--max-blank-lines=1",
                                "--reformat", -- the thing!
                                "$FILENAME",
                            },
                            to_stdin = false,
                            to_temp_file = true,
                        },
                        factory = h.formatter_factory,
                    }
                    -- FIXME: just here for the joke/test
                    local no_really_test = h.make_builtin {
                        method = nls.methods.DIAGNOSTICS,
                        filetypes = { "markdown", },
                        generator = {
                            fn = function(params)
                                local diagnostics = {}
                                for i, line in ipairs(params.content) do
                                    local col, end_col = line:find("really")
                                    if col and end_col then
                                        table.insert(diagnostics, {
                                            row = i,
                                            col = col,
                                            end_col = end_col + 1,
                                            source = "no-really",
                                            message = "Don't use 'really!'",
                                            severity = vim.diagnostic.severity.WARN,
                                        })
                                    end
                                end
                                return diagnostics
                            end,
                        },
                    }
                    return {
                        sources = {
                            -- NOTE: the LSP lua server is good for comments' alignment
                            --       if you prefer stylua, uncomment the following line
                            -- nls.builtins.formatting.stylua, -- :MasonInstall stylua
                            nls.builtins.diagnostics.eslint,
                            nls.builtins.diagnostics.twigcs,
                            -- (FIXME: restore me) nls.builtins.completion.spell,
                            -- (PHP/Symfony) :Mason insall: php-cs-fixer & phpactor
                            nls.builtins.formatting.phpcsfixer.with({
                                extra_args = {
                                    "--rules=@PhpCsFixer,@Symfony",
                                },
                            }),
                            -- (go) :Mason install: gofumpt, goimports_reviser & golines
                            -- nls.builtins.formatting.gofumpt,          -- add me when you go
                            -- nls.builtins.formatting.goimports_reviser -- add me when you go
                            -- nls.builtins.formatting.golines,          -- add me when you go
                            twig_formatter,                           -- bonus for Symfony
                            no_really_test,                           -- dummy test: load markdown with ready's
                        },
                        debug = true,
                    }
                end,
            },
        },
        opts = {
            ensure_installed = { "stylua", "jq" }, -- NOTE: stylua is ready to use but still unused here
            automatic_installation = false,
        },
    },
}
