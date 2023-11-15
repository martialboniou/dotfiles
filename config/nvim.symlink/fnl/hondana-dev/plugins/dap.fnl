;; REMINDER: TODO: llvm-vscode --> llvm-dap
(local brew-prefix
       ;; returns the status & homebrew prefix ; tested on macOS only
       #(let [handler (io.popen "brew --prefix 2>&1; echo $?")]
          (when handler
            (let [lines {}]
              (each [line (: handler :lines)]
                (table.insert lines line))
              (io.close handler)
              ;; result on one line; if not status != 0 too so ok as is (faster!)
              (let [(output status) (unpack lines)]
                (if (= :0 status)
                    (values true output)
                    (values false "error homebrew prefix not found")))))))

;; NOTE: toggle a breakpoint with `<leader>dt` (or `:DapToggleBreakpoint`)
;;       start debugging with `<leader>dc` (or `:DapContinue`; opens the UI)
;;       step into with `<leader>di` (or use the UI)
;;       stop debugging with `<leader>dx` (or `:DapTerminate`; closes the UI)
{1 :mfussenegger/nvim-dap
 :dependencies [{1 :rcarriga/nvim-dap-ui
                 :keys [{1 :<leader>dE
                         2 #(#($.eval (vim.fn.input "[Expression] > ")) (require :dapui))
                         :desc "DAP: evaluate input"}
                        {1 :<leader>dU
                         2 #(#($.toggle) (require :dapui))
                         :desc "DAP: toggle UI"}
                        ;; <leader>de is available in visual too
                        {1 :<leader>de
                         2 #(#($.eval) (require :dapui))
                         :mode [:n :v]
                         :desc "DAP: toggle UI"}]}
                {1 :ldelossa/nvim-dap-projects
                 ;; <leader>dN loads the local "per-project" adapter
                 :keys [{1 :<leader>dN
                         2 #(#($.search_project_config) (require :nvim-dap-projects))
                         :desc "DAP: loading your \"per-project\" adapter (eg: ./nvim/nvim-dap.lua)"}]}
                {1 :theHamsta/nvim-dap-virtual-text :opts {:commented true}}
                :nvim-telescope/telescope-dap.nvim
                :jbyuki/one-small-step-for-vimkind]
 :cmd [:DapContinue :DapToggleBreakpoint :DapToggleRepl]
 :keys [{1 :<leader>dR
         2 #(#($.run_to_cursor) (require :dap))
         :desc "DAP: run to cursor"}
        {1 :<leader>dC
         2 #(#($.set_breakpoint (vim.fn.input "[Condition] > ")) (require :dap))
         :desc "DAP: conditional breakpoint"}
        {1 :<leader>db
         2 #(#($.step_back) (require :dap))
         :desc "DAP: step back"}
        {1 :<leader>dc 2 #(#($.continue) (require :dap)) :desc "DAP: continue"}
        {1 :<leader>dd
         2 #(#($.disconnect) (require :dap))
         :desc "DAP: disconnect"}
        {1 :<leader>dg
         2 #(#($.session) (require :dap))
         :desc "DAP: get session"}
        {1 :<leader>di
         2 #(#($.step_into) (require :dap))
         :desc "DAP: step into"}
        {1 :<leader>do
         2 #(#($.step_over) (require :dap))
         :desc "DAP: get over"}
        {1 :<leader>dp
         2 #(#($.pause.toggle) (require :dap))
         :desc "DAP: pause"}
        {1 :<leader>dq 2 #(#($.close) (require :dap)) :desc "DAP: quit"}
        {1 :<leader>ds
         2 #(#($.continue) (require :dap))
         :desc "DAP: start (same as continue)"}
        {1 :<leader>dr
         2 #(#($.repl.toggle) (require :dap))
         :desc "DAP: toggle REPL"}
        {1 :<leader>dt
         2 #(#($.toggle_breakpoint) (require :dap))
         :desc "DAP: toggle breakpoint"}
        {1 :<leader>dx
         2 #(#($.terminate) (require :dap))
         :desc "DAP: terminate"}
        {1 :<leader>du 2 #(#($.step_out) (require :dap)) :desc "DAP: step out"}
        ;; DAP.UI.WIDGETS
        {1 :<leader>dh
         2 #(#($.hover) (require :dap.ui.widgets))
         :desc "DAP: hover variables"}
        {1 :<leader>dS
         2 #(let [widgets (require :dap.ui.widgets)
                  sidebar (widgets.sidebar widgets.scopes)]
              (sidebar.open))
         :desc "DAP: scopes"}
        ;; {1 :<leader>dS 2 #(#($.scopes) (require dap.ui.widgets)) :desc "DAP: scopes"}
        ]
 :config (λ [_ _]
           (let [{&as env} (collect [_ m (pairs [:dap :dapui])]
                             (values m (require m)))]
             (env.dapui.setup)
             (let [cfg :dapui_config
                   open #(env.dapui.open)
                   close #(env.dapui.close)]
               (tset env.dap.listeners.after :event_initialized cfg open)
               (tset env.dap.listeners.before :event_terminated cfg close)
               (tset env.dap.listeners.before :event_exited cfg close))
             ;; lldb adapter TODO: move to lldb-dap
             (local lldb-adapter-name :lldb-vscode)
             (var lldb-adapter (.. :/usr/local/bin/ lldb-adapter-name))
             (when (-> (vim.loop.os_uname)
                       (. :sysname)
                       (= :Darwin))
               ;; fetch the absolute path on homebrew
               (let [(ok brew-path) (brew-prefix)]
                 (and ok
                      (set lldb-adapter
                           (.. brew-path :/opt/llvm/bin/ lldb-adapter-name)))))
             (if (-> lldb-adapter (vim.fn.executable) (not= 1))
                 (print "error: dap: unable to set your default adapter for LLVM")
                 (let [cfg env.dap.configurations
                       ada env.dap.adapters]
                   (set ada.lldb {:type :executable
                                  :command lldb-adapter
                                  :env {:LLDB_LAUNCH_FLAG_LAUNCH_IN_TTY :YES}
                                  :name :lldb})
                   (set cfg.c
                        [{:name :Launch
                          :type :lldb
                          :request :launch
                          :cwd "${workspaceFolder}"
                          :stopOnEntry false
                          :args []
                          :runInTerminal true
                          :program #(vim.fn.input "Path to executable: "
                                                  (.. (vim.fn.getcwd) "/") :file)}])
                   (set cfg.cpp cfg.c)))))}
