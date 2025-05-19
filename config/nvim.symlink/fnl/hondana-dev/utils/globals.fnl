;; some shared data/functions
(import-macros {: tc} :hondana-dev.macros)

(local M {})

;; NOTE: cannot be cross-compiled
(macro fs-posix []
  "set a global POSIX/BSD/Unix checker"
  (-?> :Windows (= _G.jit.os) (not)))

(tc type boolean)
(set M.posix (fs-posix))

(tc type "NumberOpts[]")
;; formatted for `vim.o` (eqv. in VimL: ["nornu nonu" :nu "nu rnu"])
(local wheel [{:rnu false :nu false} {:nu true} {:nu true :rnu true}])

(tc type "integer")
;; zero-based index of wheel
;; let start with relativenumber to come ()
(var wheel-state (-> wheel (length)
                     (#(if (> $ 1) $
                           (vim.notify "utils.globals: wheel must have 2 elements!"
                                       vim.log.levels.ERROR)))
                     (- 2)))

;; roles
(tc type "Role[]")
(var roles [])

;;; DATA

;; ucm = unison-ready environment
(tc type boolean)
(set M.ucm (-> :ucm (vim.fn.executable) (= 1)))

(set M.icons (let [s vim.diagnostic.severity]
               {:diagnostic {s.ERROR "✘"
                             s.WARN "▲"
                             s.HINT " ⚑"
                             s.INFO "∴"}
                :buffer {:readonly "󰌾"
                         :modified "●"
                         :unsaved_others "○"}}))

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

;;: FUNCTIONS

(fn M.set-numbers []
  (let [last (length wheel)]
    (set wheel-state (% (+ 1 wheel-state) last))
    (local opts (. wheel (+ 1 wheel-state)))
    ;; use `vim.o` instead of `vim.cmd` to avoid VimScript call
    (each [o v (pairs opts)]
      (set (. vim.o o) v))))

(tc param tbl "string[]")
(fn M.set-roles [tbl]
  (set roles tbl))

(tc param role string return boolean)
(fn M.check-role [role]
  (vim.list_contains roles role))

M
