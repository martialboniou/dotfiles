-- :fennel:1696020727
local function _1_()
  return vim.cmd("call nvlime#plugin#Setup()")
end
return {"monkoose/nvlime", ft = "lisp", config = _1_, dependencies = "monkoose/parsley"}