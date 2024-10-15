-- :fennel:1729010657
local utils_zk = "hondana-dev.utils.zk"
local zk = require(utils_zk)
local M = {}
M.setup = function()
  local filetypes = {"markdown"}
  local trigger
  if not filetypes then
    trigger = "BufReadPost *"
  else
    trigger = ("FileType " .. table.concat(filetypes, ","))
  end
  zk._lsp_buf_auto_add(0)
  return vim.api.nvim_command(string.format(table.concat({"autocmd %s lua require'", "utils-zk", "'._lsp_buf_auto_add(0)"}), trigger))
end
local function execute_command(cmd, cb, _3foptions, _3fpath)
  do end (zk.lsp):start()
  local options
  if (_3foptions and vim.tbl_isempty(_3foptions)) then
    options = nil
  else
    options = _3foptions
  end
  local client = (zk.lsp):client()
  local bufnr = 0
  local command = ("zk." .. cmd)
  local arguments = {(_3fpath or zk["resolve-notebook-path"](bufnr)), options}
  return client.request("workspace/executeCommand", {command = command, arguments = arguments}, cb, bufnr)
end
M.list = function(cb, _3foptions, _3fpath)
  return execute_command("list", cb, _3foptions, _3fpath)
end
local function find_project_root(startpath, root_subdirectory)
  for dir in vim.fs.parents(startpath) do
    if (1 == vim.fn.isdirectory(vim.fs.joinpath(dir, root_subdirectory))) then
      return dir
    else
    end
  end
  return nil
end
local ui = {}
ui["create-note-entry-maker"] = function(_)
  local function _4_(_241)
    local title = (_241.title or _241.path)
    return {value = _241, path = _241.absPath, display = title, ordinal = title}
  end
  return _4_
end
ui["make-note-previewer"] = function()
  local previewers = require("telescope.previewers")
  local config = require("telescope.config")
  local conf = config.values
  local define_preview
  local function _5_(self, entry)
    local value = entry.value
    local bufname = (value.title or value.path)
    local winid = self.state.winid
    return conf.buffer_previewer_maker(value.absPath, self.state.bufnr, {bufname = bufname, winid = winid})
  end
  define_preview = _5_
  return previewers.new_buffer_previewer({define_preview = define_preview})
end
ui["show-note-picker"] = function(cb, notes, options)
  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")
  local actions = require("telescope.actions")
  local action_state = require("telescope.actions.state")
  local action_utils = require("telescope.actions.utils")
  local conf = require("telescope.config").values
  local action_fn
  local function _6_(prompt_bufnr)
    local function _7_()
      local v = action_state.get_selected_entry().value
      if options["multi-select"] then
        local selection = {}
        local function _8_(entry, _)
          return table.insert(selection, entry.value)
        end
        action_utils.map_selections(prompt_bufnr, _8_)
        if vim.tbl_isempty(selection) then
          selection = {v}
        else
        end
        actions.close(prompt_bufnr)
        return cb(selection)
      else
        actions.close(prompt_bufnr)
        return cb(v)
      end
    end
    return _7_
  end
  action_fn = _6_
  local attach_mappings
  local function _11_(prompt_bufnr)
    do end (actions.select_default):replace(action_fn(prompt_bufnr))
    return true
  end
  attach_mappings = _11_
  local options0 = vim.tbl_extend("force", {title = "Zk Notes"}, (options or {}))
  local picker = pickers.new({prompt_title = options0.title}, {finder = finders.new_table({results = notes, entry_maker = ui["create-note-entry-maker"](options0)}), sorter = conf.file_sorter(options0), previewer = ui["make-note-previewer"](), attach_mappings = attach_mappings})
  return picker:find()
end
M.edit = function(picker_options, options)
  local options0 = vim.tbl_extend("force", {select = {"title", "absPath", "path"}}, options)
  local function _12_(err, notes)
    assert(not err, tostring(err))
    local single_select_3f = (picker_options and (false == picker_options["multi-select"]))
    local notes0
    if single_select_3f then
      notes0 = {notes}
    else
      notes0 = notes
    end
    local function _14_(_241)
      for _, note in ipairs(_241) do
        vim.cmd(table.concat({"e", note.absPath}, " "))
      end
      return nil
    end
    return ui["show-note-picker"](_14_, notes0, picker_options)
  end
  return M.list(_12_, options0, options0.notebook_path)
end
M["back-links"] = function(_3foptions)
  local title = "Zk Backlinks"
  local linkTo = {vim.api.nvim_buf_get_name(0)}
  local o = vim.tbl_extend("force", {linkTo = linkTo}, (_3foptions or {}))
  return M.edit({title = title, ["multi-select"] = true}, o)
end
M.links = function(_3foptions)
  local title = "Zk Links"
  local linkedBy = {vim.api.nvim_buf_get_name(0)}
  local o = vim.tbl_extend("force", {linkedBy = linkedBy}, (_3foptions or {}))
  return M.edit({title = title, ["multi-select"] = true}, o)
end
return M