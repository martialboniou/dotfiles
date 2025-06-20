() {
    # REMINDER: `luarocks config lua_version 5.1` must be enabled to work with:
    #   luajit
    #   fennel-bin-luajit
    if (( $+commands[luarocks] )); then
        eval "$(luarocks path --bin)"
    fi
    # local LUAROCKS_LOCAL_PATH="${HOME}/.luarocks"
    # local LUAROCKS_LOCAL_BIN="${LUAROCKS_LOCAL_PATH}/bin"
    # if [[ -r "${LUAROCKS_LOCAL_BIN}" ]]; then
    #     export PATH="${LUAROCKS_LOCAL_BIN}:$PATH"
    # fi
}

