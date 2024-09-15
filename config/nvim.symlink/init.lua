-- the new version of this NeoVim configuration requires fennel

local function bootstrap(url)
    local name = url:gsub(".*/", "")
    local path = vim.fn.stdpath("data") .. "/lazy/" .. name

    vim.opt.rtp:prepend(path)

    if vim.fn.isdirectory(path) == 0 then
        print(name .. ": installing in data dir...")

        vim.fn.system { "git", "clone", url, path }

        vim.cmd "redraw"
        print(name .. ": finished installing")
    end
end

local udayvir_url = "https://github.com/udayvir-singh"
bootstrap(udayvir_url .. "/hibiscus.nvim")
bootstrap(udayvir_url .. "/tangerine.nvim")

local globals = vim.tbl_keys(_G)
-- example of an additional _G for tangerine.fennel
-- (say, if you need to use tangerine to compile anything):
--   table.insert(globals, "love") -- here for love2d
-- otherwise, check :FnlAddG in `fnl/hondana-dev/plugins/init.fnl`

require "tangerine".setup {
    compiler = {
        verbose = false,
        hooks = { "onsave", "oninit", },
        globals = globals,
    },
    keymaps = {
        float = {
            kill = "<C-c>",
        },
    },
    eval = {
        luafmt = function()
            local width = vim.o.colorcolumn
            return {
                "lua-format",
                "--spaces-inside-table-braces",
                "--column-table-limit", math.floor(width / 1.7),
                "--column-limit", width,
            }
        end,
    },
}
