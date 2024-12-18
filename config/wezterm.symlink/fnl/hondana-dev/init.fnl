;; REMINDER: <C-S-l> : log window (use `log_info` to print there)
;;
;; Lua 5.4 has the second value (not LuaJIT)
(local (_ file-name) ...)
(local {: log_info
        :config_builder builder
        :add_to_config_reload_watch_list watch} (require :wezterm))

;; comment the next line when debugging
(local _ log_info)
(local config (builder))
;;
;; SETUP FROM ITERM
;;
(when file-name (watch file-name))
(set config.warn_about_missing_glyphs false)
(set config.font_size 11.0)
;; KEYS
;; both option keys: esc+ (apps can change this)
;; send ^  13;2u => shift return
;; send ^  13;5u => control return
;; (local {: action} (require :wezterm))
(set config.keys [;; {:key "Return" :mods "CTRL" :action (action.SendKey {})}
                  ])

;; COLORS
;; background transparency: opaque @ 15%
(set config.window_background_opacity 0.8)
;; faint text opacity: 50
;; removes the window title & tabs
(set config.window_decorations "RESIZE")
(set config.enable_tab_bar false)
;; exit without prompt
(set config.window_close_confirmation :NeverPrompt)

config
