return {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {
        disable_filetype = {
            "TelescopePrompt",
            "minifiles", -- echasnovski/mini.files
            "vim",
        },
    },
}
