return {
    --
    "Eandrju/cellular-automaton.nvim",
    cmd = "CellularAutomaton",
    keys = {
        {
            "<leader>z",
            function()
                local animations = {}
                local ca = require("cellular-automaton")
                for k, _ in pairs(ca.animations) do
                    animations[#animations+1] = k
                end
                if animations == {} then return end
                require("cellular-automaton.manager").clean()
                local ok, _ = pcall(ca.start_animation, animations[math.random(#animations)])
                if not ok then
                    print("Cellular Automaton: cannot run in this buffer")
                end
            end,
            desc = "Procrastinate with Cellular Automaton",
        },
    },
}
