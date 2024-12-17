;; REMINDER: TODO: llvm-vscode --> llvm-dap
(import-macros {: concat!} :hibiscus.vim)
(import-macros {: tc} :hondana-dev.macros)
(import-macros {: make-lazykeys!} :hondana-dev.macros.vim)

;; F = dap functions at the end of this module
(local F {})

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

;; NOTE: toggle a breakpoint with `<leader>dt` (or `:DapToggleBreakpoint`)
;;       start debugging with `<leader>dc` (or `:DapContinue`; opens the UI)
;;       step into with `<leader>di` (or use the UI)
;;       stop debugging with `<leader>dx` (or `:DapTerminate`; closes the UI)
(local dap-lazykeys ;;
       (dap-lazykeys! [[:dR (run_to_cursor)]
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
                       [:ds (continue) "start (same as continue)"]
                       [:dr (repl.toggle) "toggle REPL"]
                       [:dt (toggle_breakpoint)]
                       [:dx (terminate)]
                       [:du (step_out)]]))

;;; KEYS
(local keys ;;
       #(let [widgets (require :dap.ui.widgets)
              sidebar (widgets.sidebar widgets.scopes)]
          [;; DAP.UI.WIDGETS
           {1 :<leader>dh 2 widgets.hover :desc "DAP: hover variables"}
           {1 :<leader>dS 2 sidebar.open :desc "DAP: scopes"}
           ;; insert additional keybindings here
           ;;
           ;; DAP standard <leader> keys
           (unpack dap-lazykeys)]))

;;; CONFIG
(fn config []
  (let [{&as env} (collect [_ m (pairs [:dap :dapui])]
                    (values m (require m)))]
    (env.dapui.setup)
    (let [cfg :dapui_config
          open #(env.dapui.open)
          close #(env.dapui.close)]
      (set (. env.dap.listeners.after.event_initialized cfg) open)
      (set (. env.dap.listeners.before.event_terminated cfg) close)
      (set (. env.dap.listeners.before.event_exited cfg) close))
    ;; lldb adapter, formely lldb-vscode
    (local lldb-adapter-name :lldb-dap)
    (var lldb-adapter (concat! "/" :/usr :local :bin lldb-adapter-name))
    (when (-> (vim.uv.os_uname)
              (. :sysname)
              (= :Darwin))
      ;; fetch the absolute path on homebrew
      (let [(ok brew-path) (F.brew-prefix)]
        (when ok
          (set lldb-adapter
               (concat! "/" brew-path :opt :llvm :bin lldb-adapter-name)))))
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
          (set cfg.cpp cfg.c)))))

;;; PLUGINS
(tc type LazySpec)
(local P {1 :mfussenegger/nvim-dap
          :dependencies [{1 :rcarriga/nvim-dap-ui
                          :dependencies :nvim-neotest/nvim-nio
                          :keys #(let [{: eval : toggle} (require :dapui)]
                                   (make-lazykeys! [[:dE
                                                     #(eval (vim.fn.input "[Expression] > "))
                                                     "DAP: evaluate input"]
                                                    [:dU
                                                     toggle
                                                     "DAP: toggle UI"]
                                                    ;; <leader>de is available in visual too
                                                    [:de
                                                     eval
                                                     {:mode [:n :v]
                                                      :desc "DAP: toggle UI"}]]))}
                         {1 :ldelossa/nvim-dap-projects
                          ;; <leader>dN loads the local "per-project" adapter
                          :keys #(let [{:search_project_config search} (require :nvim-dap-projects)]
                                   [{1 :dN
                                     2 search
                                     :desc "DAP: loading your \"per-project\" adapter (eg: ./nvim/nvim-dap.lua)"}])}
                         {1 :theHamsta/nvim-dap-virtual-text
                          :opts {:commented true}}
                         :nvim-telescope/telescope-dap.nvim
                         :jbyuki/one-small-step-for-vimkind]
          :cmd [:DapContinue :DapToggleBreakpoint :DapToggleRepl]
          : keys
          : config})

(tc type "fun(): boolean, string")
(fn F.brew-prefix []
  ;; returns the status & homebrew prefix ; tested on macOS only
  (let [handler (io.popen "brew --prefix 2>&1; echo $?")]
    (if (not handler)
        (values false "error brew command not found")
        (let [lines {}]
          (each [line (handler:lines)]
            (table.insert lines line))
          (io.close handler)
          ;; result on one line; if not status != 0 too so ok as is (faster!)
          (let [(output status) (unpack lines)]
            (if (= :0 status)
                (values true output)
                (values false "error homebrew prefix not found")))))))

P
