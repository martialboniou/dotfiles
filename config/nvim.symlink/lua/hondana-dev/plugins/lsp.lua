-- table structure by: https://github.com/MuhametSmaili/nvim/blob/main/lua/smaili/plugins/lsp/init.lua
-- LSP Zero version
-- 2023-07-15
return {
    { -- LSP Zero
        "VonHeikemen/lsp-zero.nvim",
        branch = "v2.x",
        lazy = true,
    },
    { -- LSP
        "neovim/nvim-lspconfig",
        event = "BufReadPost",
        dependencies = {
            "VonHeikemen/lsp-zero.nvim",
            {
                "williamboman/mason.nvim",
                opts = {},
                cmd = "Mason",
                run = ":MasonUpdate"
            },
            "williamboman/mason-lspconfig.nvim", -- lsp conf for mason lsp
            "hrsh7th/nvim-cmp",                  -- see Autocompletion
            "rrethy/vim-illuminate",             -- optional/highlight same word
            {
                "glepnir/lspsaga.nvim",          -- optional/fancy navbar
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
                    "rust_analyzer",
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
                    "astro",
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
            local lsp_zero = require('lsp-zero')
            local lsp_zero_setup = opts.zero_setup

            lsp_zero.nvim_workspace()

            lsp_zero.preset(lsp_zero_setup.preset)
            lsp_zero.ensure_installed(lsp_zero_setup.servers)
            lsp_zero.set_preferences(lsp_zero_setup.preferences)
            lsp_zero.set_sign_icons(lsp_zero_setup.sign_icons) -- or .sign_chars

            lsp_zero.on_attach(function(_, bufnr)
                local options = { buffer = bufnr, remap = false }

                vim.keymap.set("n", "<leader>f", function() vim.lsp.buf.format() end, options)
                vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, options)
                vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, options)
                vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end, options)
                vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, options)
                vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, options)
                vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, options)
                vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end, options)
                vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end, options)
                vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, options)
                vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, options)
                -- ! for ergonomics : <leader> + ca = vca, rr = vrr, nn = vrn
                vim.keymap.set("n", "<leader>ca", function() vim.lsp.buf.code_action() end, options)
                vim.keymap.set("n", "<leader>rr", function() vim.lsp.buf.references() end, options)
                vim.keymap.set("n", "<leader>nn", function() vim.lsp.buf.rename() end, options)
            end)

            require('lspconfig').lua_ls.setup(lsp_zero.nvim_lua_ls())

            lsp_zero.setup(opts)
        end,
    },
    { -- Autocompletion
        "hrsh7th/nvim-cmp",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            {
                "L3MON4D3/LuaSnip",
                opts = {},
                build = "make install_jsregexp",
            },
            "rafamadriz/friendly-snippets", -- optional
            "saadparwaiz1/cmp_luasnip",
            "hrsh7th/cmp-buffer",           -- optional/buffer words
            "hrsh7th/cmp-path",             -- optional/filesystem paths
            "ray-x/lsp_signature.nvim",
            -- "hrsh7th/cmp-nvim-lsp-signature-help" -- replaced by previous
        },
        opts = function()
            -- nvim-lspconfig ensures the lazy loading of LSP Zero
            require('lsp-zero.cmp').extend()

            local cmp = require("cmp")
            -- local cmp_action_from_lsp_zero = require('lsp-zero.cmp').action()

            -- vim.opt.runtimepath:append("~/github/lsp_signature.nvim")

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
                    { name = "buffer" },
                    { name = "path" },
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
                    --       I chose <C-q> as <C-a> is cursor navi in terms
                    -- other keys: <C-u><C-d> = scroll; <C-e> = cancel
                    ["<Tab>"] = nil,
                    ["<S-Tab>"] = nil,
                }),
            }
        end,
    },
    { -- Formatter
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
                    return {
                        sources = {
                            nls.builtins.formatting.stylua,
                            nls.builtins.diagnostics.eslint,
                            nls.builtins.completion.spell,
                            -- (optional) for my Symfony backend
                            -- :Mason insall: php-cs-fixer & phpactor
                            nls.builtins.formatting.phpcsfixer.with({
                                extra_args = {
                                    "--rules=@PhpCsFixer,@Symfony"
                                },
                            }),
                        },
                        debug = true,
                    }
                end,
            },
        },
        opts = {
            ensure_installed = { 'stylua', 'jq' },
            automatic_installation = false,
        },
        config = function()
            require("luasnip.loaders.from_vscode").lazy_load()
        end,
    },
}
