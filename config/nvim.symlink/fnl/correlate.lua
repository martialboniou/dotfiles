if not _G["vim"] then
	error("you cannot execute this without nvim")
end
-- use fennel from the tangerine plugin installed in the lazy data directory
local tangerine_path = vim.fn.stdpath("data") .. "/lazy/tangerine.nvim"
_G["vim"].opt.rtp:prepend(tangerine_path)
local ok, fennel = pcall(require, "tangerine.fennel.latest")
if not ok then
	error("you must bootstrap tangerine.nvim first; run nvim at least once")
end
table.insert(package.loaders or package.searchers, fennel.make_searcher({ correlate = true }))
-- from there your Fennel modules are like Lua code
