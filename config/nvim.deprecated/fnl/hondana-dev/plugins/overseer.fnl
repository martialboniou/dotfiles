(local keys [{1 :<leader>ow 2 "<Cmd>OverseerToggle<CR>" :desc "Task list"}
             {1 :<leader>oo 2 "<Cmd>OverseerRun<CR>" :desc "Run task"}
             {1 :<leader>ot
              2 "<Cmd>OverseerTaskAction<CR>"
              :desc "Task action"}])

(local cmd [:OverseerToggle
            :OverseerRun
            :OverseerTaskAction
            :OverseerOpen
            :OverseerShell])

(local render #(let [r (require :overseer.render)]
                 (r.format_verbose $)))

(local opts {:templates [:hondana_zig_task] :task_list {: render}})

{1 :stevearc/overseer.nvim
 ;; version 2.0.0+
 : opts
 : cmd
 : keys}

;; NOTE: `:OverseerTaskAction` to view the task, you can manually edit the
;; buffer but you must save it, say, with `:w`
