return
    {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v2.x',
        lazy = true,
        config = function()
            local lsp = require('lsp-zero')
            lsp.preset("recommended")
            lsp.ensure_installed({
                'tsserver',
                'eslint',
                'lua_ls',
                'rust_analyzer',
            })
            -- TODO: check assert above
            -- lsp.nvim_workspace() -- fix undefined global 'vim'
        end,
    },
    -- Autocompletion
    {
        'hrsh7th/nvim-cmp',
        event = 'InsertEnter',
        dependencies = {
            'hrsh7th/cmp-path',         -- :before
            'hrsh7th/cmp-buffer',       -- :before
            'saadparwaiz1/cmp_luasnip', -- :before
            {
                'L3MON4D3/LuaSnip',
                dependencies = {
                    'rafamadriz/friendly-snippets', -- optional
                },
            },
        },
        config = function()
            -- autocompletion settings
            local lsp = require('lsp-zero.cmp')
            lsp.extend()
            local cmp = require('cmp')
            local cmp_select = { behavior = cmp.SelectBehavior.Select }
            local cmp_mappings = lsp.defaults.cmp_mappings({
                ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
                ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
                ['<C-y>'] = cmp.mapping.confirm({ select = true }),
                ["<C-Space>"] = cmp.mapping.complete(),
            })

            cmp_mappings['<Tab>'] = nil
            cmp_mappings['<S-Tab>'] = nil

            lsp.setup_nvim_cmp({
                mapping = cmp_mappings,
            })

            cmp.setup({ -- TODO: check this
                sources = {
                    {
                        name = 'buffer',
                        option = {
                            keyword_pattern = [[\k\+]],
                        },
                    },
                },
            })
        end -- config =
    },
    -- LSP
    {
        'neovim/nvim-lspconfig',
        cmd = 'LspInfo',
        event = { 'BufReadPre', 'BufNewFile' },
        dependencies = {
            'hrsh7th/nvim-cmp', -- added (FIXME: needed rework deps)
            'hrsh7th/cmp-nvim-lsp',
            'williamboman/mason-lspconfig.nvim',
            {
                'williamboman/mason.nvim',
                build = function()
                    pcall(vim.cmd, 'MasonUpdate')
                end,
            },
        },
        config = function()
            local lsp = require('lsp-zero')

            lsp.set_sign_icons({
                error = '✘',
                warn = '▲',
                hint = '⚑',
                info = '»',
            })

            lsp.set_preferences({
                suggest_lsp_servers = true, -- CHECK: false
            })

            lsp.on_attach(function(_, bufnr)
                local opts = { buffer = bufnr, remap = false }
                vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
                vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
                vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end, opts)
                vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, opts)
                vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
                vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
                vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end, opts)
                vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end, opts)
                vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, opts)
                vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)
            end)

            -- lua language server
            require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())
            --[[ other examples
            require("lspconfig").tsserver.setup({})
            require("lspconfig").eslint.setup({})
            require("lspconfig").rust_analyzer.setup({})
            ]]
            lsp.setup()

            -- from ThePrimeagen - 2023/07/12
            -- https://github.com/ThePrimeagen/init.lua/blob/master/after/plugin/lsp.lua
            vim.diagnostic.config({
                virtual_text = true
            })
        end,
    },
}
