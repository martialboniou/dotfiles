;; REMINDER: TODO: llvm-vscode --> llvm-dap
(import-macros {: concat!} :hibiscus.vim)
(import-macros {: funcall!} :hondana-dev.macros)

(macro dap-lazykeys! [lkeys]
  (assert-compile (sequence? lkeys) "expected table for keys")
  (icollect [_ lkey (ipairs lkeys)]
    (let [[key expr ?desc ?mode] lkey]
      (assert-compile (list? expr) "expected s-exp for expr")
      (icollect [_ v (ipairs [key ?desc])]
        (assert-compile (= :string (type v))
                        "expected string for the first AND third arguments"))
      (let [[f & args] expr]
        `{1 ,(.. :<leader> key)
          2 #((#(. $ ,(tostring f)) (require :dap)) ,(unpack args))
          :desc ,(.. "DAP: " (or ?desc (-> f (tostring) (: :gsub "_" " "))))
          :mode ,(or ?mode :n)}))))

(lua "---@type fun(): boolean, string")
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
(lua "---@type LazySpec")
(local dap {1 :mfussenegger/nvim-dap
            :dependencies [{1 :rcarriga/nvim-dap-ui
                            :keys [{1 :<leader>dE
                                    2 #(funcall! :dapui :eval
                                                 (vim.fn.input "[Expression] > "))
                                    :desc "DAP: evaluate input"}
                                   {1 :<leader>dU
                                    2 #(funcall! :dapui :toggle)
                                    :desc "DAP: toggle UI"}
                                   ;; <leader>de is available in visual too
                                   {1 :<leader>de
                                    2 #(funcall! :dapui :eval)
                                    :mode [:n :v]
                                    :desc "DAP: toggle UI"}]}
                           {1 :ldelossa/nvim-dap-projects
                            ;; <leader>dN loads the local "per-project" adapter
                            :keys [{1 :<leader>dN
                                    2 #(funcall! :nvim-dap-projects
                                                 :search_project_config)
                                    :desc "DAP: loading your \"per-project\" adapter (eg: ./nvim/nvim-dap.lua)"}]}
                           {1 :theHamsta/nvim-dap-virtual-text
                            :opts {:commented true}}
                           :nvim-telescope/telescope-dap.nvim
                           :jbyuki/one-small-step-for-vimkind]
            :cmd [:DapContinue :DapToggleBreakpoint :DapToggleRepl]
            :keys [;; DAP.UI.WIDGETS
                   {1 :<leader>dh
                    2 #(funcall! :dap.ui.widgets :hover)
                    :desc "DAP: hover variables"}
                   {1 :<leader>dS
                    2 #(let [widgets (require :dap.ui.widgets)
                             sidebar (widgets.sidebar widgets.scopes)]
                         (sidebar.open))
                    :desc "DAP: scopes"}
                   ;; insert additional keybindings here
                   ;;
                   ;; DAP standard <leader> keys
                   (unpack (dap-lazykeys! [[:dR (run_to_cursor)]
                                           [:dC
                                            (set_breakpoint (vim.fn.input "[Condition] > "))
                                            "conditional breakpoint"]
                                           [:db (step_back)]
                                           [:dc (continue)]
                                           [:dd (disconnect)]
                                           [:dg (session) "get session"]
                                           [:di (step_into)]
                                           [:do (step_over)]
                                           [:dp (pause.toggle) :pause]
                                           [:dq (close) :quit]
                                           [:ds
                                            (continue)
                                            "start (same as continue)"]
                                           [:dr (repl.toggle) "toggle REPL"]
                                           [:dt (toggle_breakpoint)]
                                           [:dx (terminate)]
                                           [:du (step_out)]]))]
            :config #(let [{&as env} (collect [_ m (pairs [:dap :dapui])]
                                       (values m (require m)))]
                       (env.dapui.setup)
                       (let [cfg :dapui_config
                             open #(env.dapui.open)
                             close #(env.dapui.close)]
                         (tset env.dap.listeners.after :event_initialized cfg
                               open)
                         (tset env.dap.listeners.before :event_terminated cfg
                               close)
                         (tset env.dap.listeners.before :event_exited cfg close))
                       ;; lldb adapter TODO: move to lldb-dap
                       (local lldb-adapter-name :lldb-vscode)
                       (var lldb-adapter
                            (concat! "/" :/usr :local :bin lldb-adapter-name))
                       (when (-> (vim.uv.os_uname)
                                 (. :sysname)
                                 (= :Darwin))
                         ;; fetch the absolute path on homebrew
                         (let [(ok brew-path) (brew-prefix)]
                           (and ok
                                (set lldb-adapter
                                     (concat! "/" brew-path :opt :llvm :bin
                                              lldb-adapter-name)))))
                       (if (-> lldb-adapter (vim.fn.executable) (not= 1))
                           (print "error: dap: unable to set your default adapter for LLVM")
                           (let [cfg env.dap.configurations
                                 ada env.dap.adapters]
                             (set ada.lldb
                                  {:type :executable
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
                                                            (.. (vim.fn.getcwd)
                                                                "/")
                                                            :file)}])
                             (set cfg.cpp cfg.c))))})

dap
