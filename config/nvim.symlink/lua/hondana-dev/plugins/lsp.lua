-- table structure by: https://github.com/MuhametSmaili/nvim/blob/main/lua/smaili/plugins/lsp/init.lua
-- 2023-07-12
return {
    { -- LSP
        "neovim/nvim-lspconfig",
        event = "BufReadPost",
        dependencies = {
            { "williamboman/mason.nvim", opts = {}, run = ":MasonUpdate" },
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
            servers = {
                "tsserver",
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
            icons = {
                diagnostics = {
                    Error = "✘",
                    Warn = "▲",
                    Hint = "⚑",
                    Info = "»",
                },
            },
        },
        config = function(_, opts)
            -- diagnostics
            for name, icon in pairs(opts.icons.diagnostics) do
                name = "DiagnosticSign" .. name
                vim.fn.sign_define(name, { text = icon, texthl = name, numhl = "" })
            end
            vim.diagnostic.config(opts.diagnostics)
            --  servers {name, config} -- TODO: separate files for each server

            local servers = {
                "lua_ls",
            }

            -- smaili:
            -- for _, file in
            -- ipairs(
            -- vim.fn.readdir(vim.fn.stdpath("config") .. "/lua/smaili/plugins/lsp/servers", [[v:val =~ '\.lua$']])
            -- )
            -- do
            --     local server = require("smaili.plugins.lsp.servers." .. file:gsub("%.lua$", ""))
            --     -- lsp.configure(server.name, server.config)
            --     servers[server.name] = server.config
            -- end

            ----------------------------------
            -- ON ATTACH
            ----------------------------------
            local on_attach = function(_, bufnr)
                ----------------------------------
                -- Load key mappings
                ----------------------------------
                require("hondana-dev.mappings.lsp.map")(bufnr) -- smaili: added to replace the nextline
                -- require("smaili.plugins.lsp.key-mappings")(bufnr)

                -- Highlight lsp reference when we keep hovering -> :h document_highlight
                -- if client.server_capabilities.documentHighlightProvider then
                -- 	vim.api.nvim_create_augroup("lsp_document_highlight", { clear = true })
                -- 	vim.api.nvim_clear_autocmds({ buffer = bufnr, group = "lsp_document_highlight" })
                -- 	vim.api.nvim_create_autocmd("CursorHold", {
                -- 		callback = vim.lsp.buf.document_highlight,
                -- 		buffer = bufnr,
                -- 		group = "lsp_document_highlight",
                -- 		desc = "Document Highlight",
                -- 	})
                -- 	vim.api.nvim_create_autocmd("CursorMoved", {
                -- 		callback = vim.lsp.buf.clear_references,
                -- 		buffer = bufnr,
                -- 		group = "lsp_document_highlight",
                -- 		desc = "Clear All the References",
                -- 	})
                -- end
            end

            ----------------------------------
            -- Add servers automaticlly
            ----------------------------------
            local capabilities =
                require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())
            require("mason-lspconfig").setup({ ensure_installed = opts.servers })
            require("mason-lspconfig").setup_handlers({
                function(server_name)
                    local server_opts = servers[server_name] or {}
                    server_opts["capabilities"] = capabilities
                    server_opts["on_attach"] = on_attach
                    require("lspconfig")[server_name].setup(server_opts)
                end,
            })
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
            local cmp = require("cmp")
            local luasnip = require("luasnip")
            vim.opt.runtimepath:append("~/github/lsp_signature.nvim")

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
                formatting = {
                    format = function(_, item)
                        -- smaili: next line
                        -- item.kind = string.format("%s %s", smaili.icons.lsp[item.kind], item.kind)
                        return item
                    end,
                },
                sources = cmp.config.sources({
                    -- { name = "nvim_lsp_signature_help" }, --
                    { name = "nvim_lsp" },
                    { name = "luasnip" },
                    { name = "buffer" },
                    { name = "path" },
                }),
                -- experimental = { ghost_text = true },
                mapping = cmp.mapping.preset.insert({
                    ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
                    ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
                    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-f>"] = cmp.mapping.scroll_docs(4),
                    ["<C-Space>"] = cmp.mapping.complete({}),
                    ["<C-y>"] = cmp.mapping.confirm({ select = true }),
                }),
            }
        end,
    },
    { -- Formatter
        "jay-babu/mason-null-ls.nvim",
        event = { "BufReadPost", "BufNewFile" },
        dependencies = {
            "williamboman/mason.nvim",
            "jose-elias-alvarez/null-ls.nvim", -- frozen?
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
