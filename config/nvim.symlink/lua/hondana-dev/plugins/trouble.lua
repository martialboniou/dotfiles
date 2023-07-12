return {
    "folke/trouble.nvim",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    keys = {
        {
            silent = true,
            noremap = true,
            "<leader>xx",
            "<cmd>TroubleToggle quickfix<CR>",
            desc = "Toogle trouble quickfix",
        },
    },
}
