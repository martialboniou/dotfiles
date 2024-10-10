-- :fennel:1728578749
local Lsp = {}
Lsp.__index = Lsp
Lsp.new = function(self, name, _3fconfig)
  local config = (_3fconfig or {})
  return setmetatable({name = name, config = config, ["_client-id"] = nil}, self)
end
Lsp["external-client"] = function(self)
  local ac_symbol
  if (1 == vim.fn.has("nvim-0.10")) then
    ac_symbol = "get_clients"
  else
    ac_symbol = "get_active_clients"
  end
  local active_clients = vim.lsp[ac_symbol]({name = self.name})
  if (nil ~= next(active_clients)) then
    for _, v in ipairs(active_clients) do
      if (nil ~= next(v.attached_buffers)) then
        return v.id
      else
      end
    end
    return nil
  else
    return nil
  end
end
Lsp.start = function(self, _3fconfig)
  if _3fconfig then
    self.config = _3fconfig
  else
  end
  if not self["_client-id"] then
    self["_client-id"] = self["external-client"](self)
  else
  end
  if not self["_client-id"] then
    self["_client-id"] = vim.lsp.start_client(self.config)
    return nil
  else
    return nil
  end
end
Lsp["buf-add"] = function(self, _3fbufnr)
  self:start()
  return vim.lsp.buf_attach_client((_3fbufnr or 0), self._client_id)
end
Lsp.stop = function(self)
  local client = self:client()
  if client then
    client.stop()
  else
  end
  self["_client-id"] = nil
  return nil
end
Lsp.client = function(self)
  return vim.lsp.get_client_by_id(self["_client-id"])
end
Lsp["get-name"] = function(self)
  return self.name
end
return Lsp