{1 :mfussenegger/nvim-dap
 :cmd [:DapToggleBreakpoint :DapToggleRepl]
 :keys [{1 :<leader>db
         2 #(#($.toggle_breakpoint) (require :dap))
         :desc "Add breakpoint at line"}
        {1 :<leader>dus
         2 #(let [widgets (require :dap.ui.widgets)
                  sidebar (widgets.sidebar widgets.scopes)]
              (sidebar.open))
         :desc "Open debugging sidebar"}]}
