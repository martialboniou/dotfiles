(local keys [{1 :<leader>ow 2 "<Cmd>OverseerToggle<CR>" :desc "Task list"}
             {1 :<leader>oo 2 "<Cmd>OverseerRun<CR>" :desc "Run task"}
             {1 :<leader>oq
              2 "<Cmd>OverseerQuickAction<CR>"
              :desc "Action recent task"}
             {1 :<leader>oi 2 "<Cmd>OverseerInfo<CR>" :desc "Overseer Info"}
             {1 :<leader>ob 2 "<Cmd>OverseerBuild<CR>" :desc "Task builder"}
             {1 :<leader>ot
              2 "<Cmd>OverseerTaskAction<CR>"
              :desc "Task action"}
             {1 :<leader>oc
              2 "<Cmd>OverseerClearCache<CR>"
              :desc "Clear cache"}])

(local cmd [:OverseerToggle
            :OverseerRun
            :OverseerQuickAction
            :OverseerInfo
            :OverseerBuild
            :OverseerTaskAction
            :OverseerClearCache
            :OverseerOpen])

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
