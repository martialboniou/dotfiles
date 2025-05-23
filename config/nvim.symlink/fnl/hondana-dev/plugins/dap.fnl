;; REMINDER: TODO: llvm-vscode --> llvm-dap
;; TODO: WIP: testing since lsp rework (2025-05-21)
(import-macros {: tc} :hondana-dev.macros)
(import-macros {: make-lazykeys!} :hondana-dev.macros.vim)

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

;; fun(): boolean, string
(macro brew-prefix []
  ;; returns the status & homebrew prefix; tested on macOS only
  (let [{: popen : close} _G.io
        handler (popen "brew --prefix 2>&1; echo $?")]
    (if (not handler)
        (values false "error brew command not found")
        (let [lines {}]
          (each [line (handler:lines)]
            (table.insert lines line))
          (close handler)
          ;; result on one line; if not status != 0 too so ok as is (faster!)
          (let [(output status) (unpack lines)]
            (if (= :0 status)
                `(values true ,output)
                `(values false "error homebrew prefix not found")))))))

;;; KEYS

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

(tc return "string[]|string|fun(self:LazyPlugin,ft:string[]):string[]")
(fn keys []
  (let [widgets (require :dap.ui.widgets)
        sidebar (widgets.sidebar widgets.scopes)]
    [;; DAP.UI.WIDGETS
     {1 :<leader>dh 2 widgets.hover :desc "DAP: hover variables"}
     {1 :<leader>dS 2 sidebar.open :desc "DAP: scopes"}
     ;; insert additional keybindings here
     ;;
     ;; DAP standard <leader> keys
     (unpack dap-lazykeys)]))

(tc return "string[]|string|fun(self:LazyPlugin,ft:string[]):string[]")
(fn dapui-keys []
  (let [{: eval : toggle} (require :dapui)]
    (make-lazykeys! [[:dE
                      #(eval (vim.fn.input "[Expression] > "))
                      "DAP: evaluate input"]
                     [:dU toggle "DAP: toggle UI"]
                     ;; <leader>de is available in visual too
                     [:de eval {:mode [:n :v] :desc "DAP: toggle UI"}]])))

(tc return "string[]|string|fun(self:LazyPlugin,ft:string[]):string[]")
(fn dap-projects-keys []
  (let [{:search_project_config search} (require :nvim-dap-projects)]
    [{1 :dN
      2 search
      :desc "DAP: loading your \"per-project\" adapter (eg: ./nvim/nvim-dap.lua)"}]))

(tc param exe string)
(tc return "string?")
(fn full-exe-path [exe]
  (-> exe (vim.fn.exepath) (#(if (= "" $) nil $))))

;;; CONFIG
(tc type "fun(self:LazyPlugin, opts:table): nil")
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
    (local lldb-adapter-name :lldb-dap)
    (var lldb-adapter (or (full-exe-path lldb-adapter-name) ;; legacy
                          (full-exe-path "lldb-vscode")))
    (when (-> (vim.uv.os_uname)
              (. :sysname)
              (= :Darwin))
      ;; fetch the absolute path of homebrew at comptime
      (let [(ok brew-path) (brew-prefix)]
        (when ok
          (set lldb-adapter
               (vim.fs.joinpath brew-path :opt :llvm :bin lldb-adapter-name)))))
    (tc cast lldb_adapter string)
    (if (-> lldb-adapter (vim.fn.executable) (not= 1))
        (vim.notify "dap: unable to set your default adapter for LLVM; upgrade or check path"
                    vim.log.levels.WARN)
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
                          :keys dapui-keys}
                         {1 :ldelossa/nvim-dap-projects
                          ;; <leader>dN loads the local "per-project" adapter
                          :keys dap-projects-keys}
                         {1 :theHamsta/nvim-dap-virtual-text
                          :opts {:commented true}}
                         :nvim-telescope/telescope-dap.nvim
                         :jbyuki/one-small-step-for-vimkind]
          :cmd [:DapContinue :DapToggleBreakpoint :DapToggleRepl]
          : keys
          : config})

P
