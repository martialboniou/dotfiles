-- :fennel:1690999479
local function lisp_ft_3f(ft)
    _G.assert((nil ~= ft), "Missing argument ft on hondana-dev/plugins/brackets.fnl:4")
    local __val__ = ft
    return ((__val__ == "lisp") or (__val__ == "scheme") or (__val__ == "fennel") or (__val__ == "shen") or (__val__ == "clojure"))
end
local function lisp_rules(tag)
    _G.assert((nil ~= tag), "Missing argument tag on hondana-dev/plugins/brackets.fnl:7")
    local Rule = require("nvim-autopairs.rule")
    local cond = require("nvim-autopairs.conds")
    local function _1_()
        return not lisp_ft_3f(vim.bo.filetype)
    end
    return Rule(tag, tag):with_pair(cond.not_before_regex_check("%w")):with_pair(_1_)
end
local function _2_(_, opts)
    _G.assert((nil ~= opts), "Missing argument opts on hondana-dev/plugins/brackets.fnl:25")
    local pairs = require("nvim-autopairs")
    pairs.setup(opts)
    for _, char in ipairs({ "'", "`" }) do
        pairs.remove_rule(char)
        pairs.add_rule(lisp_rules(char))
    end
    return nil
end
return {
    { "windwp/nvim-autopairs", event = "InsertEnter",
        opts = { disable_filetype = { "TelescopePrompt", "minifiles", "vim" }, enable_check_bracket_line = false },
        config = _2_ }, { "kylechui/nvim-surround", event = "VeryLazy", config = true },
    { "wellle/targets.vim", event = "VeryLazy" } }
