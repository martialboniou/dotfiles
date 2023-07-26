-- check treesitter.lua and lsp.lua for additional settings
-- search for `add me when you go`
return {
    {
        -- :Mason install: delve
        "dreamsofcode-io/nvim-dap-go",
        ft = "go",
        keys = {
            {
                "<leader>dgt",
                function()
                    require("dap-go").debug_test()
                end,
                desc = "Debug go test",
            },
            {
                "<leader>dgl",
                function()
                    require("dap-go").debug_last()
                end,
                desc = "Debug last go test",
            },
        },
        dependencies = {
            "mfussenegger/nvim-dap",
        },
    },
    {
        "olexsmir/gopher.nvim",
        ft = "go",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-treesitter/nvim-treesitter",
        },
        keys = {
            -- BEWARE: <leader>gs = git status (fuGITive)
            {
                "<leader>gtj", -- memo: gopher tag JSON
                function()
                    vim.cmd [[ GoTagAdd json ]]
                end,
                desc = "Add JSON struct tags",
            },
            {
                "<leader>gty", -- memo: gopher tag YAML
                function()
                    vim.cmd [[ GoTagAdd yaml ]]
                end,
                desc = "Add YAML struct tags",
            },
        },
        build = function()
            vim.cmd [[ silent! GoInstallDeps ]]
        end,
    },
}
