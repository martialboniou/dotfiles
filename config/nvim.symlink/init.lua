-- this NeoVim configuration is written in Fennel
-- the plugin `tangerine.nvim` will take care of the compilation
local joinpath = vim.fs.joinpath
local stdpath = vim.fn.stdpath

local nvim_dir = stdpath([[config]])

---@param url string
local function bootstrap(url)
  --
  local name = url:gsub(".*/", "")
  local path = joinpath(stdpath("data"), "lazy", name)

  ---@diagnostic disable-next-line: undefined-field
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
local hondana_url = "https://github.com/martialboniou"
bootstrap(udayvir_url .. "/hibiscus.nvim")
bootstrap(hondana_url .. "/tangerine.nvim")

---@type TangerineLuaGlobals
local globals = vim.tbl_keys(_G)
-- example of an additional _G for tangerine.fennel
-- (say, if you need to use Tangerine to compile anything):
--   table.insert(globals, "love") -- here for love2d
-- otherwise, check :FnlAddG in `fnl/hondana-dev/plugins/init.fnl`

---@param ... string
---@return string
local function root(...)
  return joinpath(nvim_dir, ...)
end

---@param ... string
---@return string
local function fnl(...)
  return root("fnl", ...)
end

---@type fun(config: TangerineConfig)
local function setup(config)
  require("tangerine").setup(config)
end

setup({
  -- the default `init.fnl` has been renamed to `boot.fnl` as
  -- a newcomer may erase the mandatory file `init.lua` while
  -- using a command like `:FnlCompile`
  vimrc = fnl("boot.fnl"),
  -- don't auto-recompile the `fnl/` files but those inside the
  -- `hondana-dev` subdirectory; use the `custom` option to add
  -- your own code
  source = fnl("hondana-dev"),
  target = root("lua", "hondana-dev"),
  -- lsp custom configuration files since nvim 0.11
  custom = { { fnl("lsp"), root("lsp") }, { fnl("after", "lsp"), root("after", "lsp") } },
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
    eval_buffer = "gB", -- the default `gE` jumps backwards to the end of a word (w/ or w/o punctuation)
    goto_output = "gG", -- the default `gO` is `vim.lsp.buf.document_symbol()` since 0.11
    -- NOTE: float.kill = <Esc> AND <C-c> (see below)
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

-- I want to kill the peek buffer on <C-c> AND <Esc> (default setup)
-- HACK: rewrite a major function to add <C-c> to `:FnlWinKill`
local has_helper, helper = pcall(require, "hondana-dev.utils.boot-helper")
if has_helper then
  helper["tangerine-new-create-float"]()
end

---@class TangerineFloatKeymaps
---@field next? string
---@field prev? string
---@field kill? string
---@field close? string
---@field resizef? string
---@field resizeb? string
---@class TangerineKeymaps
---@field eval_buffer string
---@field peek_buffer? string
---@field goto_output? string
---@field float? TangerineFloatKeymaps
---@alias TangerineLuaGlobals string[]
---@alias TangerineHook 'onsave'|'onload'|'oninit'
---@class TangerineCompilerConfig
---@field float? boolean
---@field clean? boolean
---@field force? boolean
---@field verbose? boolean
---@field globals? TangerineLuaGlobals
---@field adviser? fun(fennel: any): any
---@field version? string
---@field hooks? TangerineHook[]
---@class TangerineCustomPair
---@field [1] string
---@field [2] string
---@class TangerineHighlightConfig
---@field float? string
---@field success? string
---@field errors? string
---@class TangerineConfig
---@field vimrc? string
---@field source? string
---@field target? string
---@field rtpdirs? string[]
---@field custom? TangerineCustomPair[]
---@field compiler? TangerineCompilerConfig
---@field keymaps? TangerineKeymaps
---@field highlight? TangerineHighlightConfig
