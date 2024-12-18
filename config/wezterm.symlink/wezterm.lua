local wezterm = require("wezterm")
debug = {
  traceback = function() end,
}

-- there should be a copy of a recent fennel script here (via these dotfiles)
local fennel_script = table.concat({
  os.getenv("HOME"),
  ".config",
  "nvim",
  "fnl",
  "fennel.lua",
}, "/")
local has_fennel, fennel = pcall(dofile, fennel_script, "r")
local config

if has_fennel then
  -- fennel version: check the `fnl/hondana-dev` subdirectory
  do
    local configdir = wezterm.config_dir
    local fnldir = configdir .. "/fnl"
    for _, dir in ipairs({ "/?.fnl", "/?/init.fnl" }) do
      fennel.path = fnldir .. dir .. ";" .. fennel.path
      fennel.macroPath = fnldir .. dir .. ";" .. fennel.macroPath
    end
    fennel.macroPath = fnldir .. "/?/init-macros.fnl" .. ";" .. fennel.macroPath
    fennel["macro-path"] = fennel.macroPath
  end
  fennel.install()
  config = require("hondana-dev")
else
  -- minimal version if fennel.lua not found
  config = wezterm.config_builder()
  config.font_size = 11.0
  config.window_background_opacity = 0.8
  -- removes the window title & tabs
  config.window_decorations = "RESIZE"
  config.enable_tab_bar = false
  -- exit without prompt
  config.window_close_confirmation = "NeverPrompt"
end

return config
