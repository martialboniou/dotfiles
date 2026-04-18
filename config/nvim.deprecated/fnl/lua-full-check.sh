#!/usr/bin/env bash

nvim_dir="${HOME}/.config/nvim"
# lua "${nvim_dir}/lua"
# lsp "${nvim_dir}/lsp"
# after_lsp="${nvim_dir}/after/lsp"
# fetch every Lua files with any type annotations
files=$(rg -uu -tlua --json "\-\-\-\@" "${nvim_dir}" |\
    jq -r 'select(.type == "begin") | .data.path.text' | xargs)
[[ -z "${files}" ]] || \
    # `bufdo e` to reload for context/LSP; then execute trouble with a timer
    nvim +"args ${files}" +"bufdo e" +"lua do local timer = vim.uv.new_timer(); timer:start(2000, 0, vim.schedule_wrap(function () vim.cmd(\"Trouble diagnostics toggle focus=true\") end)) end"
echo
