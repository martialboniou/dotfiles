return {
    "folke/trouble.nvim",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    keys = {
        {
            "<leader>xq",
            "<cmd>TroubleToggle quickfix<CR>",
            silent = true,
            noremap = true,
            desc = "Toogle trouble quickfix",
        },
    },
}
