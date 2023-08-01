return {
    "ThePrimeagen/refactoring.nvim",
    cmd = { "Refactor" },
    opts = {
        prompt_func_return_type = {
            java = true,
            php  = true,
            lua  = true,
            ts   = true,
        },
        prompt_func_param_type = {
            java = true,
            php  = true,
            lua  = true,
            ts   = true,
        },
        printf_statements = {},
        print_var_statements = {},
    },
    dependencies = {
        "nvim-lua/plenary.nvim",
        "nvim-treesitter/nvim-treesitter",
    },
    keys = {
        {
            "<leader>re",
            mode = "x",
            function() require("refactoring").refactor("Extract Function") end,
            desc = "Refactor: extract function",
        },
        {
            "<leader>rf",
            mode = "x",
            function() require("refactoring").refactor("Extract Function To File") end,
            desc = "Refactor: extract function to a file",
        },
        {
            "<leader>rv",
            mode = "x",
            function() require("refactoring").refactor("Extract Variable") end,
            desc = "Refactor: extract variable",
        },
        {
            "<leader>ri",
            { mode = { "x", "n", }, },
            function() require("refactoring").refactor("Inline Variable") end,
            desc = "Refactor: inline variable",
        },
        {
            "<leader>rb",
            function() require("refactoring").refactor("Extract Block") end,
            desc = "Refactor: extract block",
        },

        {
            "<leader>rbf",
            function() require("refactoring").refactor("Extract Block To File") end,
            desc = "Refactor: extract block to a file",
        },
        -- ! for ergonomics : <leader> + rbb = rbf
        {
            "<leader>rbb",
            function() require("refactoring").refactor("Extract Block To File") end,
            desc = "Refactor: extract block to a file",
        },
    },
}
