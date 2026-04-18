-- this script is required by build-flsproject.sh
-- NOTE: in current NeoVim 0.10+, `-l` option doesn't work (stderr/!stdout pipe)
-- so, in order to use `-u` option instead, you need THIS Lua script
require("correlate") --[[mandatory to load Fennel code as Lua code]]
require("flsproject-generator")["create-command"]()
