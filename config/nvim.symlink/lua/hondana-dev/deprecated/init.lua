-- (DEPRECATED) this file is for the fennel free version only
require("hondana-dev.deprecated.remap")
require("hondana-dev.deprecated.set")

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

-- bootstrap lazy
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath,
    })
end

vim.opt.rtp:prepend(lazypath)

local opts = {
    spec = {
        { import = "hondana-dev.plugins.transition" }, -- handwritten lua plugin specs
        -- NOTE: these settings aren't maintained anymore (2023-08-01)
    },
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

require("lazy").setup(opts)
