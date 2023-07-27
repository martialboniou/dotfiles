require("hondana-dev/remap")
require("hondana-dev/set")

local lazy_path = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
local hotpot_path = lazy_path .. '/hotpot.nvim'

-- bootstrap lazy
if not vim.loop.fs_stat(lazy_path) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazy_path,
    })
end

vim.opt.rtp:prepend(lazy_path)

local opts = {
    defaults = { lazy = true },
    checker = { enabled = false },
    performance = {
        rtp = {
            disabled_plugins = {
                "tutor",
            },
        },
    }
}

require("lazy").setup("hondana-dev.plugins", opts)

-- bootstrap hotpot
if not vim.loop.fs_stat(hotpot_path) then
    vim.notify('Bootstrapping hotpot.nvim...', vim.log.levels.INFO)
    vim.fn.system({
        'git',
        'clone',
        '--filter=blob:none',
        '--single-branch',
        'https://github.com/rktjmp/hotpot.nvim.git',
        hotpot_path,
    })
end

vim.opt.rtp:prepend(hotpot_path)

require('hotpot').setup({
    provide_require_fennel = true,
    compiler = {
        modules = {
            correlate = true,
        },
    },
})
