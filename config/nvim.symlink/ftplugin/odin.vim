" Vim filetype plugin file
" Language: Odin
" Last Change: 2025 Sep 08 by hondana@gmx.com
"
" This file extends the default plugin by habamax@gmail.com published here:
"  https://github.com/neovim/neovim/runtime/ftplugin
if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

" as detected by Sleuth
setlocal noet
setlocal ts=4
setlocal sw=0
