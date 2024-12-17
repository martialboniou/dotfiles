local wezterm = require("wezterm")
local config = wezterm.config_builder()

-- config.color_scheme = 'Batman'
config.font_size = 11.0

-- setup from iTerm
-- KEYS
-- -- both option keys: esc+ (apps can change this)
-- -- send ^[[13;2u => shift return
-- -- send ^[[13;5u => control return
-- -- NOTE: seems to work out of the box
config.keys = {
  -- { key = "Return", mods = "CTRL", action = wezterm.action.SendKey {  }, },
}
-- COLORS
-- -- background transparency: opaque @ 15%
config.window_background_opacity = 0.8
-- -- faint text opacity: 50

return config
