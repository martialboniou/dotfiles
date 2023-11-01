-- :fennel:1691241401
local function _1_()
  local function _2_(_2410)
    return _2410.debug_test()
  end
  return _2_(require("dap-go"))
end
local function _3_()
  local function _4_(_2410)
    return _2410.debug_last()
  end
  return _4_(require("dap-go"))
end
local function _5_()
  return vim.cmd("GoTagAdd json")
end
local function _6_()
  return vim.cmd("GoTagAdd yaml")
end
local function _7_()
  return vim.cmd(" silent! GoInstallDeps ")
end
return {{"dreamsofcode-io/nvim-dap-go", ft = "go", keys = {{"<leader>dgt", _1_, desc = "Debug go test"}, {"<leader>dgl", _3_, desc = "Debug last go test"}}, dependencies = {"mfussenegger/nvim-dap"}}, {"olexsmir/gopher.nvim", ft = "go", dependencies = {"nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter"}, keys = {{"<leader>gtj", _5_, desc = "Add JSON struct tags"}, {"<leader>gty", _6_, desc = "Add YAML struct tags"}}, build = _7_}}