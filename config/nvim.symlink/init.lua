-- the new version of this NeoVim configuration requires fennel
-- if you want to boot the previous lua-only version,
-- set the following to false
local tangerine_dream = true
if not tangerine_dream then
    require "hondana-dev.deprecated"
else
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

    require "tangerine".setup {
        compiler = {
            verbose = false,
            hooks = { "onsave", "oninit", },
        },
        keymaps = {
            float = {
                kill = "<C-c>",
            },
        },
    }
end
