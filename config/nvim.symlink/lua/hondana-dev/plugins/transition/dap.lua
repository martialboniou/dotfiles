return {
    "mfussenegger/nvim-dap",
    keys = {
        {
            "<leader>db",
            function()
                require("dap").toggle_breakpoint()
            end,
            desc = "Add breakpoint at line",
        },
        {
            "<leader>dus",
            function()
                local widgets = require("dap.ui.widgets")
                local sidebar = widgets.sidebar(widgets.scopes);
                sidebar.open()
            end,
            desc = "Open debugging sidebar",
        },
    },
    cmd = { "DapToggleBreakpoint", "DapToggleRepl", },
}
