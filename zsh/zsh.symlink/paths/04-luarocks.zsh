() {
    # REMINDER: `luarocks config lua_version 5.1` must be enabled to work with:
    #   luajit
    #   fennel-bin-luajit
    if (( $+commands[luarocks] )); then
        eval "$(luarocks path --bin)"
    fi
}

