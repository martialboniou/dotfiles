(λ run-random-automaton []
  (local ca (require :cellular-automaton))
  (local animations (vim.tbl_keys ca.animations))
  (when (not= animations {})
    (let [(ok _) (pcall ca.start_animation
                        (->> animations
                             (length)
                             (math.random)
                             (. animations)))]
      (when (not ok)
        (print "Cellular Automaton: cannot run in this buffer")))))

{1 :Eandrju/cellular-automaton.nvim
 :cmd :CellularAutomaton
 :keys [{1 :<leader>zz
         2 #(run-random-automaton)
         :desc "Procrastinate with Cellular Automaton"}]}
