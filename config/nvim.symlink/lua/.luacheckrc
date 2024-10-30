std = luajit
-- cache = true
codes = true

ignore = {
    "211/_[^0-9]*.*", -- unused variable/function is OK when starting with an underscore
    "212/%.%.%.", -- unused variable length argument (from Fennel hashfn)
    "212/_.*", -- unused argument is OK when starting with an underscore
    "213/_.*", -- unused loop variable is OK when starting with an underscore
    "231/_[^0-9]*.*", -- variable is never accessed
    "541", -- empty do...end block (from Fennel closures)
    "542", -- empty if branch (from Fennel when/if)
    "631", -- line is too long (from Fennel raw output; DON'T TRY TO FIX THIS!)
}
globals = {
    "vim",
}

read_globals = { }
