local wezterm = require("wezterm")
debug = {
  traceback = function() end,
}

-- there should be a copy of a recent fennel script here (via these dotfiles)
local has_fennel, fennel = pcall(dofile, "/Users/mars/.config/nvim/fnl/fennel.lua", "r")
local config

if has_fennel then
  -- fennel version: check the `fnl/hondana-dev` subdirectory
  do
    local configdir = wezterm.config_dir
    wezterm.log_error(configdir)
    local fnldir = configdir .. "/fnl"
    for _, dir in ipairs({ "/?.fnl", "/?/init.fnl" }) do
      fennel.path = fnldir .. dir .. ";" .. fennel.path
      fennel["macro-path"] = fnldir .. dir .. ";" .. fennel["macro-path"]
    end
    fennel["macro-path"] = fnldir .. "/?/init-macros.fnl" .. ";" .. fennel["macro-path"]
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
