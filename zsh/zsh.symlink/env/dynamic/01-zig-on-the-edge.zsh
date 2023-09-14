() {
    local ZIG_UNSTABLE_PATH="${HOME}/.local/share/zig-unstable"
    if [[ -r "${ZIG_UNSTABLE_PATH}" ]]; then
        export PATH="${ZIG_UNSTABLE_PATH}/bin:$PATH"
    fi
}
