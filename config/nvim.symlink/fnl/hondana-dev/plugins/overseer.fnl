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

;; TODO: learn how to upgrade to 2.0.0 AKA master
{1 :stevearc/overseer.nvim
 :tag "v1.6.0"
 ;; :commit "2c23513" 
 :opts {:templates [:zig_task]}
 : cmd
 : keys}
