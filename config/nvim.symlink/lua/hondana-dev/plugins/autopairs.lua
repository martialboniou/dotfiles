return {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {
        disable_filetype = {
            "TelescopePrompt",
            "minifiles", -- echasnovski/mini.files
            "vim",
        },
        enable_check_bracket_line = false, -- very annoying for Lisp
    },
    config = function(_, opts)
        local pairs = require("nvim-autopairs")
        local cond = require("nvim-autopairs.conds")
        local Rule = require("nvim-autopairs.rule")

        pairs.setup(opts)

        -- lisp exceptions for quotes & backticks -- TODO: check this, deadline: 3 weeks
        for _, char in ipairs({ "'", "`", }) do
            pairs.remove_rule(char)
            pairs.add_rule(
                Rule(char, char)
                :with_pair(cond.not_before_regex_check("%w"))
                :with_pair(function()
                    return vim.bo.filetype ~= "lisp"
                        and vim.bo.filetype ~= "scheme"
                        and vim.bo.filetype ~= "fennel"
                        and vim.bo.filetype ~= "shen"
                        and vim.bo.filetype ~= "clojure"
                end))
        end
    end,
}
