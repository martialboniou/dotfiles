# What?

Some useful dotfiles for developers using applications such as Zsh, Tmux,
(Neo)Vim... on POSIX systems. Used mainly on macOS and linux.

# For whom?

Me and my roomies. But anyone can find something cool here.
Notice I use a simplified **Dvorak layout** on my keyboard: expect
some HTNS keybindings and others optimized for this layout. 
(The HJKL keybindings stay unchanged in (Neo)Vim and their plugins.)

# How?

Use the [easy](https://github.com/holman/dotfiles) way:

    mkdir -p ~/Documents/Code
    git clone git@gitlab.com:hondana-dev/dotfiles
    cd !$/dots && script/bootstrap

# And?

## Config directory

Every files/directories in `config/` will be linked to the `$HOME` directory as a subdirectory of `.config/` instead of having their symlink in the `.<filename>` form.

## NeoVim (up-to-date, my editor)

- Minimal configuration written in [Fennel](https://fennel-lang.org)
  (keybindings are made for dvorak keyboards)
- `rg` (AKA riggrep) should be installed to use the Grep options (in Telescope)
- [Lazy](https://github.com/folke/lazy.nvim) (`nvim .` should load Netrw and
  a colorscheme only; everything loads when needed)
- [Tangerine](https://github.com/udayvir-singh/tangerine.nvim) (enable the
  NeoVim setup based on Fennel scripts; compiles and brings all the REPL commands
  to easily develop in Fennel) 
- Telescope (used as fuzzy finder) & [Harpoon 2](https://github.com/ThePrimeagen/harpoon/tree/harpoon2)
- [mini.files](https://github.com/echasnovski/mini.files) (very good to navigate,
  preview files and manage directories; replace Netrw even if it's still available
  thru `:Ex`; use `<leader><leader>` to open/close it at your current location)
- Undotree & Fugitive (same in my classic Vim setup)
- [Treesitter](https://github.com/nvim-treesitter/nvim-treesitter) (interface
  to a fast parser generator tool and incremental parsing library) &
  [ts-comments.nvim](https://github.com/folke/ts-comments.nvim) (a tiny
  plugin to enhance Neovim's native comments; for example, it supports
  different comment strings for different Treesitter node types) 
- Mason-boosted LSP & CMP ([zero](https://github.com/VonHeikemen/lsp-zero.nvim)
  helped!). [null-ls](https://github.com/jose-elias-alvarez/null-ls.nvim) is
  used for additional formatters, diagnostic tools & for spellchecking.
  Standard keybindings:
  - for completion:
    - `<C-space>`     : force to start
    - `<C-y>`/`<C-e>` : confirm/cancel
    - `<C-p>`/`<C-n>` : previous/next option
    - `<C-d>`/`<C-u>` : go down/up in documentation
  - for snippets:
    - `<C-f>`/`<C-b>` : move forward/backward in the selection
- [Debugger Adapter Protocol](https://github.com/mfussenegger/nvim-dap) with
  an embedded UI and LLDB adapter probably ready to use
- others:
  - [refactoring](https://github.com/ThePrimeagen/refactoring.nvim) &
  [trouble](https://github.com/folke/trouble.nvim)
  - [rainbow delimiters](https://github.com/HiPhish/rainbow-delimiters.nvim)
    (additional colors for brackets)
  - [mini.hues](https://github.com/echasnovski/mini.hues) (**minimalist
    colorscheme**; 5 colors now: gray comment, white foreground, 3 *hue-wise*
    complementary color (same saturation, same brightness); works well
    with TreeSitter and LSP semantics (except in `clangd` whose semantics
    have been disabled))
  - some various plugins for *word objects*, *brackets* and
    *symbolic expressions*:
    - [nvim-surround](https://github.com/kylechui/nvim-surround) classic
      quotes/brackets manipulation;
    - [vim-exchange](https://github.com/tommcdo/vim-exchange) easy text
      exchange operator
    - **WIP**:
      - try paredit/parinfer solution for Lisp (paredit activated now)
      - disabled ones:
          - [targets](https://github.com/wellle/targets.vim) for additional text
            objects (dangerous; broke the *Vim* macros (recorded & typed));
          - [autopairs](https://github.com/windwp/nvim-autopairs) (disabled in
            Telescope & mini.files; quotes and backticks are skipped from
            *autopairing* for Lisp languages)
- sources:
  - [Tangerine configuration](https://github.com/Massolari/neovim)
  ([Fennel](https://fennel-lang.org/) scripts and Lazy plugin specs)
  - [good lazy configuration](https://github.com/MuhametSmaili/nvim) (inspiration
  for Lazy installation of LSP, Autocompletion and Formatter)
  - [0 to LSP : Neovim RC From Scratch](https://www.youtube.com/watch?v=w7i4amO_zaE)
  (smooth vanilla NeoVim tutorial based on `packer`; inspiration for settings/keybindings)
- tested on macOS (aarch) using NeoVim 0.9, tmux & iTerm2 (should work with 0.8)

NOTE: I'm new to NeoVim (June 2023; I use it before but Vim 7+ was good enough).
I decided to start from *scratch*. Still have some functions to convert from
VimScript to Lua.

## Zsh (my shell)

Because the best shell make the most beautiful pearls. Zsh (by default on macOS) is a better Bourne with every fancy things from `csh` like _aliases_. Forget [Bash](http://www.bash2zsh.com/) ! And get a better shell with smarter completions.

For example, some functions are useful to navigate in directories:

-   Ctrl-U: go **up** in directory;
-   Ctrl-P: go to the **previous** visited directory;

are great bindings to change directory without touching your command line (and so letting ugly `cd ..` in your history).

The starting point is the `.zshenv` then visit:

-   `.zsh/zshrc.d/` subdirectory for environment setting and functions' configuration;
-   `.zsh/env/` subdirectory for `path` customization.

Notice that the numbers in front of file names show you the loading **order** like on a lot GNU Debian classical library setup in `/etc`.

`tmux` must be installed in order to use `.zsh/zshrc.d/99-tmux-sessions.zsh`.
On macOS, install the following package (see [tmux-MacOSX-pasteboard](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard)):

```sh
brew install reattach-to-user-namespace
```

**IMPORTANT**: some useful functions require `fzf` and `fd`

## Tmux (my terminal manager)

- Ensure the version (`tmux -V`) is 3.1+
- `fn a` is the prefix when skhd is activated on macOS; `F5` otherwise.
  I use `<C-a>` and `<C-e>` to navigate in a terminal (move cursor at the
  beginning and end of the current line respectively). `<C-b>` was the default
  but it is in conflict with the *snippet backward* default keybinding in
  (Neo)Vim.

## Yabai (my additional macOS window manager)

- simple setup
- notification works as expected but you need to check the
[official documentation](https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(latest-release))
to configure the scripting addition at startup
- the window manipulation requires to start `skhd` as a service:
  - the keybindings are based on the `fn` key (instead of `Option`:
    the `M-x`, `M-w`, `M-y`, `M--`, `M-n` `M-p`... Emacs keybindings were
    too complicated to manage); BEWARE: `fn a` is a mapping alias for the
    tmux prefix (`F5`)
  - use the touchpad or additional keybindings to move between Spaces (the
    default Yabai space navigation doesn't work without breaking the SIP)

## Vim 8 (archived)

The packages will be auto-installed via vim-plug. Untested on NeoVim. Install FZF and yarn if possible.

## GNU Emacs

### Emacs 29 (used for Common Lisp projects)

- starts on a *Sly* session (requires `sbcl`)
- minimal setup
- Evil mode
- (TODO: add Org, mu4e... and other good tools that don't exist elsewhere)

### Previous version (obsolete)

All scripts in `.emacs.d/2013-scripts` are currently tested on Emacs 24.3.
You can access to this archive from this  
[revision](https://github.com/martialboniou/dotfiles/tree/db66a2a41cd4fc950a378088de126f5ffef67a19).

## Bat

- add a `~/.config/bat/syntaxes/Fennel.sublime-syntax` for
  [fennel](https://fennel-lang.org) code's syntax highlighting
  in [bat](https://github.com/sharkdp/bat}
- you MUST execute `~/.config/bat/BUILD.sh` to refresh the cache 
