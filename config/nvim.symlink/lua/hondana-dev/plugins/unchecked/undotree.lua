return { -- NOTE: more reliable than debugloop/telescope-undo.nvim
	'mbbill/undotree',
    cmd = "UndotreeToggle",
    keys = {
        { "<leader>u", vim.cmd.UndotreeToggle,
            desc = "Toggle the undo-tree panel"
        },
    },
}
