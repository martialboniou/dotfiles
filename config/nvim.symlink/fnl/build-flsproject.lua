-- this script is required by build-flsproject.sh
-- NOTE: in current NeoVim 0.10+, `-l` option doesn't work (stderr/!stdout pipe)
-- so, in order to use `-u` option instead, you need THIS Lua script
table.insert(package.loaders, require("fennel").make_searcher({ correlate = true }))
-- from here your Fennel modules are like Lua code
require("flsproject-generator")["create-command"]()
