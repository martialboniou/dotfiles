-- the new version of this NeoVim configuration requires fennel
local nvim_dir = vim.fn.stdpath([[config]])

---@type fun(url: string)
local function bootstrap(url)
	local name = url:gsub(".*/", "")
	local path = vim.fn.stdpath("data") .. "/lazy/" .. name

	vim.opt.rtp:prepend(path)

	if vim.fn.isdirectory(path) == 0 then
		print(name .. ": installing in data dir...")

		vim.fn.system({ "git", "clone", url, path })

		vim.cmd("redraw")
		print(name .. ": finished installing")
	end
end

---@type string
local udayvir_url = "https://github.com/udayvir-singh"
bootstrap(udayvir_url .. "/hibiscus.nvim")
bootstrap(udayvir_url .. "/tangerine.nvim")

---@type string[]
local globals = vim.tbl_keys(_G)
-- example of an additional _G for tangerine.fennel
-- (say, if you need to use Tangerine to compile anything):
--   table.insert(globals, "love") -- here for love2d
-- otherwise, check :FnlAddG in `fnl/hondana-dev/plugins/init.fnl`

require("tangerine").setup({
	-- the default `init.fnl` has been renamed to `boot.fnl` as
	-- a newcomer may erase the mandatory file `init.lua` while
	-- using a command like `:FnlCompile`
	vimrc = nvim_dir .. "/boot.fnl",
	-- don't auto-recompile the `fnl/` files but those inside the
	-- `hondana-dev` subdirectory; use the `custom` option to add
	-- your own code
	source = nvim_dir .. "/fnl/hondana-dev",
	target = nvim_dir .. "/lua/hondana-dev",
	compiler = {
		verbose = false,
		hooks = { "onsave", "oninit" },
		globals = globals,
		-- Fennel self-contained libraries shouldn't compile on Luajit non-compatable 5.2
		-- - I recommend to compile with the `fennel` script using Lua (or luajit with 5.2
		--   compatibility)
		-- - I recommend to separate the macro libraries from the modules (and name them
		--   according to the pattern `*-macros.fnl` to avoid the auto-compilation by
		--   `tangerine.nvim`)
		-- - I also use a neovim 0.10 based on luajit with 5.2 compatibility;
		--   so I can compile these self-contained libraries from `tangerine.nvim`
		--   without errors: this `adviser` function adds the required `requiredAsInclude`
		--   option; useful to embed both Fennel & Lua modules (as a self-contained
		--   library has Fennel macros inside a module)
		adviser = function(fennel)
			---@diagnostic disable-next-line:deprecated
			table.insert(package.loaders or package.searchers, fennel.make_searcher({ requireAsInclude = true }))
			return fennel
		end,
	},
	keymaps = {
		eval_buffer = "gB",
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
				"--column-table-limit",
				math.floor(width / 1.7),
				"--column-limit",
				width,
			}
		end,
	},
})
