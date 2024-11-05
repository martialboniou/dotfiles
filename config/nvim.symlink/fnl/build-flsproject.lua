-- this script is required by build-flsproject.sh
-- NOTE: in current NeoVim 0.10+, `-l` option doesn't work (stderr/!stdout pipe)
-- so, in order to use `-u` option instead, you need THIS Lua script
if not _G.vim then
	error("you cannot execute this outside of nvim")
end
-- use fennel from the tangerine plugin installed in the lazy data directory
local tangerine_path = vim.fn.stdpath("data") .. "/lazy/tangerine.nvim"
_G.vim.opt.rtp:prepend(tangerine_path)
local ok, fennel = pcall(require, "tangerine.fennel.latest")
if not ok then
	error("you must bootstrap tangerine.nvim first; run nvim at least once")
end
table.insert(package.loaders or package.searchers, fennel.make_searcher({ correlate = true }))
-- from here your Fennel modules are like Lua code
require("flsproject-generator")["create-command"]()
