require'nvim-treesitter.configs'.setup {

  ensure_installed = { "typescript", "javascript", "c", "lua", "vim", "vimdoc", "rust" },

  sync_install = false,

  auto_install = true,

  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}
