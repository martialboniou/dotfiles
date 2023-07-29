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
    config = function(opts)
        local pairs = require("nvim-autopairs")
        pairs.setup(opts)
        local rules = require("nvim-autopairs.rule")
        -- lisp/rust exceptions
        local lispem = { "lisp", "fennel", "scheme", "shen", }
        pairs.get_rules("`")[1].not_filetypes = lispem
        table.insert(lispem, "rust")
        pairs.get_rules("'")[1].not_filetypes = lispem
        -- pairs.get_rules("'")[1]:with_pair(cond.not_after_text("[")) -- TODO: test this
        -- tex
        pairs.add_rule(rules("$$", "$$", "tex"))
    end,
}
