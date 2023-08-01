return {
    "zbirenbaum/copilot.lua",
    cmd = { "Copilot" },
    keys = {
        { "<M-]>", mode = "i" }, -- next suggestion
    },
    opts = function()
        vim.api.nvim_create_user_command(
            "CopilotTrigger",
            function()
                require("copilot.suggestion").toggle_auto_trigger()
            end, {}) -- NOTE: if cmp disturbs you, use <C-e>/<C-Space> to switch off/back
        return {
            suggestion = {
                keymap = {
                    accept = "<M-=>",      -- was <M-l> but used by yabai (memo: = to sync as in mini.files)
                    accept_word = "<C-]>", -- may be useful!
                    dismiss = "<C-[>",
                    next = "<M-]>",
                    prev = "<M-[>",
                },
            },
        }
    end,
}
