(λ run-random-automaton []
  (let [{: animations : start_animation} (require :cellular-automaton)
        {: clean} (require :cellular-automaton.manager)
        animations (vim.tbl_keys animations)]
    (clean)
    (let [(ok _) (pcall start_animation
                        (->> animations
                             (length)
                             (math.random)
                             (. animations)))]
      (when (not ok)
        (print "Cellular Automaton: cannot run in this buffer")))))

(lua "---@type LazySpec")
(local P ;;
       {1 :Eandrju/cellular-automaton.nvim
        :cmd :CellularAutomaton
        :keys [{1 :<leader>zz
                2 run-random-automaton
                :desc "Procrastinate with Cellular Automaton"}]})

P
