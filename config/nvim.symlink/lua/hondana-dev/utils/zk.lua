-- :fennel:1728984520
local Lsp = require("hondana-dev.utils.lsp")
local root_pattern = require("hondana-dev.utils.root-pattern")
local M = {}
do
  local name_root = "zk"
  local name = (name_root .. "-mini")
  local cmd = {name_root, "lsp"}
  M.lsp = Lsp:new(name, {name = name, cmd = cmd})
end
M._lsp_buf_auto_add = function(bufnr)
  if (("nofile" ~= vim.api.nvim_buf_get_option(bufnr, "buftype")) and M["notebook-root"](vim.api.nvim_buf_get_name(bufnr))) then
    return (M.lsp):buf_add(bufnr)
  else
    return nil
  end
end
M["notebook-root"] = function(n_path)
  return root_pattern["root-pattern"](".zk")(n_path)
end
M["get-notebook-global-path"] = function()
  local config_file = vim.fn.expand("~/.config/zk/config.toml")
  local output = vim.fn.system(string.format(table.concat({"taplo", "get", "-f", "\"%s\"", "notebook.dir"}, " "), config_file))
  local dir = vim.fn.expand(output:match("(.+)\n"))
  if (vim.fn.isdirectory(dir) == 1) then
    return M["notebook-root"](dir)
  else
    return nil
  end
end
M["resolve-notebook-path"] = function(_3fbufnr)
  local path = vim.api.nvim_buf_get_name((_3fbufnr or 0))
  local cwd = vim.fn.getcwd(0)
  if ("" == path) then
    path = cwd
  else
  end
  if not M["notebook-root"](path) then
    if M["notebook-root"](cwd) then
      path = cwd
    else
      local notebook_dir = M["get-notebook-global-path"]()
      if notebook_dir then
        path = notebook_dir
      else
      end
    end
  else
  end
  return path
end
return M