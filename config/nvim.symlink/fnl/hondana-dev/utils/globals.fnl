(import-macros {: tc} :hondana-dev.macros)

(local M {})

;; NOTE: cannot be cross-compiled
(macro fs-posix []
  "set a global POSIX/BSD/Unix checker"
  (-?> :Windows (= _G.jit.os) (not)))

(tc type boolean)
(set M.posix (fs-posix))

(set M.icons
     {:diagnostic {vim.diagnostic.severity.ERROR "✘"
                   vim.diagnostic.severity.WARN "▲"
                   vim.diagnostic.severity.HINT "⚑"
                   vim.diagnostic.severity.INFO "»"}
      :buffer {:readonly "󰌾" :modified "●" :unsaved_others "○"}})

(set M.modes {:n {:text nil :state :normal}
              :niI {:text nil :state :normal}
              :niR {:text nil :state :normal}
              :niV {:text nil :state :normal}
              :nt {:text nil :state :normal}
              :ntT {:text nil :state :normal}
              :no {:text :O-PENDING :state :pending}
              :nov {:text :O-PENDING :state :pending}
              :noV {:text :O-PENDING :state :pending}
              :no\22 {:text :O-PENDING :state :normal}
              :v {:text :VISUAL :state :visual}
              :vs {:text :VISUAL :state :visual}
              :S {:text :S-LINE :state :visual}
              :V {:text :V-LINE :state :visual}
              :Vs {:text :V-LINE :state :visual}
              "\22" {:text :V-BLOCK :state :visual}
              "\22s" {:text :V-BLOCK :state :visual}
              "\19" {:text :S-BLOCK :state :visual}
              :s {:text :SELECT :state :visual}
              :S {:text :S-LINE :state :visual}
              :i {:text :INSERT :state :insert}
              :ix {:text :INSERT :state :insert}
              :ic {:text :INSERT :state :insert}
              :r {:text :PROMPT :state :replace}
              :R {:text :REPLACE :state :replace}
              :Rx {:text :REPLACE :state :replace}
              :Rc {:text :REPLACE :state :replace}
              :Rv {:text :V-REPLACE :state :replace}
              :Rvx {:text :V-REPLACE :state :replace}
              :Rvc {:text :V-REPLACE :state :replace}
              :rm {:text :MORE :state :command}
              :r? {:text :CONFIRM :state :command}
              :c {:text :COMMAND :state :command}
              :cv {:text :EX :state :command}
              :ce {:text :EX :state :command}
              :! {:text :SHELL :state :command}
              :t {:text :TERMINAL :state :command}})

M
