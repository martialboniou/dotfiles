-- :fennel:1728396654
local M = {}
do
  local is_fs_root
  local function _1_(_241)
    if false then
      return _241:match("^%a:$")
    else
      return ("/" == _241)
    end
  end
  is_fs_root = _1_
  local dirname
  local function _3_(path)
    local strip_dir_pat = "/([^/]+)$"
    local strip_sep_pat = "/$"
    if (path and (0 ~= #path)) then
      local result = path:gsub(strip_sep_pat, ""):gsub(strip_dir_pat, "")
      if (0 == #result) then
        if false then
          return path:sub(1, 2):upper()
        else
          return "/"
        end
      else
        return result
      end
    else
      return nil
    end
  end
  dirname = _3_
  local function _7_(_241)
    return _241:gsub("([%[%]%?*])", "\\%1")
  end
  local function _8_(_241)
    local stat = vim.loop.fs_stat(_241)
    return ((stat and stat.type) or false)
  end
  local function _9_(...)
    return table.concat(vim.tbl_flatten({...}), "/")
  end
  local function _10_(_241)
    local it
    local function _11_(_, v)
      if (v and not is_fs_root(v)) then
        local dv = dirname(v)
        if (dv and vim.loop.fs_realpath(v)) then
          return v, _241
        else
          return nil
        end
      else
        return nil
      end
    end
    it = _11_
    return it, _241, _241
  end
  M.path = {["escape-wildcards"] = _7_, exists = _8_, join = _9_, ["iterate-parents"] = _10_}
end
M["search-ancestors"] = function(startpath, func)
  vim.validate({func = {func, "f"}})
  if func(startpath) then
    return startpath
  else
    local guard = 100
    for path in M.path["iterate-parents"](startpath) do
      guard = (guard - 1)
      if (0 == guard) then
        return
      else
      end
      if func(path) then
        return path
      else
      end
    end
    return nil
  end
end
M["root-pattern"] = function(...)
  local patterns = vim.tbl_flatten({...})
  local matcher
  local function _17_(path)
    for _, pattern in ipairs(patterns) do
      for _0, p in ipairs(vim.fn.glob(M.path.join(M.path["escape-wildcards"](path), pattern), true, true)) do
        if M.path.exists(p) then
          return path
        else
        end
      end
    end
    return nil
  end
  matcher = _17_
  local function _19_(_241)
    return M["search-ancestors"](_241, matcher)
  end
  return _19_
end
return M