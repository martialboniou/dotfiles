# What?

Some useful dotfiles for developers using applications such as Zsh, Tmux,
(Neo)Vim... on POSIX systems. Used mainly on macOS, OpenBSD and Linux.

# For whom?

Me and my roomies. But anyone can find something cool here.
Notice I use a simplified **Dvorak layout** on my keyboard: expect
some HTNS keybindings and others optimized for this layout. 
(The HJKL keybindings stay unchanged in (Neo)Vim and their plugins.)

# How?

Use the [easy](https://github.com/holman/dotfiles) way (`bash` required):

    mkdir -p ~/Documents/Code
    git clone git@gitlab.com:hondana-dev/dotfiles
    cd !$/dots && script/bootstrap

# And?

## Config directory

Every files/directories in `config/` will be linked to the `$HOME` directory as a subdirectory of `.config/` instead of having their symlink in the `.<filename>` form.

## NeoVim 0.11+ (up-to-date, my editor)

- Minimal configuration written in [Fennel](https://fennel-lang.org)
  (keybindings are made for dvorak keyboards)
- `rg` (AKA riggrep) should be installed to use the Grep options (in Telescope); ensure some
  tools are available on your POSIX operating system (others like `taplo` or `jq` might be
  auto-installed via [Mason](https://github.com/mason-org/mason.nvim)):
  - `curl`, `wget`, `gzip` & `unzip` (**MANDATORY** for Mason; they might be missing in some
    minimal Linux distributions like `macpine` after the default setup; if you miss the
    `~/.local/share/nvim/mason/bin` folder, try `:checkhealth mason` in NeoVim first)
  - `bash`
  - `git`
  - `clang`
  - `make`
  - `nodejs npm` & `npm install -g tree-sitter-cli node-gyp`
    - `g++` (only for `awk-language-server` on Linux Alpine)
  - `lldb` (for `dap`)
  - (optional but recommended for `blink.cmp`) [`rustup`](https://rustup.rs)
    - `cargo` & a recent `rust` compiler (`rustc@1.79` minimum)
    - I can recommend the `brew` version on macOS (otherwise install manually or via `nix`)
  - (optional for `fennel` or this NeoVim setup maintenance)
  [`fennel-ls`](https://git.sr.ht/~xerool/fennel-ls)/[`fnlfmt`](https://git.sr.ht/~technomancy/fnlfmt)
- `fzf` & `fd` is highly recommended
- [Lazy](https://github.com/folke/lazy.nvim) (`nvim .` should load Netrw and
  a colorscheme only; everything loads when needed)
- [Tangerine](https://github.com/udayvir-singh/tangerine.nvim) (enable the
  NeoVim setup based on Fennel scripts; compiles and brings all the REPL commands
  to easily develop in Fennel; **BEWARE**: the default `gE` keybinding used to evaluate
  a buffer or selection is `gB` in this setup; you should probably only use `gL`,
  `gC` or `gO` to *peek*, compile to or check the Lua output respectively) 
    - NOTE: since end of June 2025, I will use my fork and try to fix some
    compatibility issues; works on 0.11 & 0.12 (deprecation removed and
    recompiled without headers)
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
- Mason & `blink.cmp` setup.
  [conform](https://github.com/stevearc/conform.nvim) &
  [nvim-lint](https://github.com/mfussenegger/nvim-lint) are used
  for additional formatters & diagnostic tools respectively.
  Standard keybindings:
  - for completion (TODO: might change!):
    - `<C-space>`     : force to start
    - `<C-y>`/`<C-e>` : confirm/cancel
    - `<C-p>`/`<C-n>` : previous/next option
    - `<C-d>`/`<C-u>` : go down/up in documentation
  - for snippets:
    - `<C-f>`/`<C-b>` : move forward/backward in the selection
- [match-up](https://github.com/andymass/vim-matchup) to improve the text
  matching with `%` (including parens & tags) with Treesitter integration;
  no more issues with parens matched inside strings or comments in your Lisp
  code
- [Debugger Adapter Protocol](https://github.com/mfussenegger/nvim-dap) with
  an embedded UI and LLDB adapter probably ready to use
- others:
  - [zk](https://github.com/zk-org/zk-nvim)
    ([Zettelkasten](https://github.com/zk-org/zk) note taking tool)
  - [refactoring](https://github.com/ThePrimeagen/refactoring.nvim),
  [trouble](https://github.com/folke/trouble.nvim) &
  [todo-comments](https://github.com/folke/todo-comments.nvim)
  - [rainbow delimiters](https://github.com/HiPhish/rainbow-delimiters.nvim)
    (additional colors for brackets)
  - [mini.hues](https://github.com/echasnovski/mini.hues) (**minimalist
    colorscheme**; 5 colors now: gray comment, white foreground, 3 *hue-wise*
    complementary color (same saturation, same brightness); works well
    with Treesitter and LSP semantics (except in `clangd` whose semantics
    have been disabled))
  - some various plugins for *word objects*, *brackets* and
    *symbolic expressions*:
    - [nvim-surround](https://github.com/kylechui/nvim-surround) classic
      quotes/brackets manipulation;
    - [vim-exchange](https://github.com/tommcdo/vim-exchange) easy text
      exchange operator
    - [paredit](https://github.com/kovisoft/paredit) is enabled for all
      Lisp code (including the Treesitter Query Editor)
    - [which-key](https://github.com/folke/which-key.nvim) as a key bindings'
      sticker
    - **Work in progress**:
      - TODO: try other paredit/parinfer solution for Lisp; a Emacs puni-like
        plugin for NeoVim with Treesitter support and the following current
        [paredit](https://github.com/kovisoft/paredit) tricks would be neat:
        - auto-whitespace before any opening inner bracket (Lisp wants
          `(() ())` not `(()())`)
        - smooth deletion
        - electric return
      - disabled ones:
        - [autopairs](https://github.com/windwp/nvim-autopairs) (disabled in
          Telescope & mini.files; quotes and backticks are skipped from
          *autopairing* for Lisp languages; can be activated if
          [nvim-paredit](https://github.com/julienvincent/nvim-paredit) is too)
        - [targets](https://github.com/wellle/targets.vim) for additional text
          objects (dangerous; broke the *Vim* macros (recorded & typed));
- sources (mainly **obsolete** content as since NeoVim 0.11 the LSP setup is easier and
  [`nvim-lspconfig`](https://github.com/neovim/nvim-lspconfig) is optional):
  - [Tangerine configuration](https://github.com/Massolari/neovim)
  ([Fennel](https://fennel-lang.org/) scripts and Lazy plugin specs)
  - [good lazy configuration](https://github.com/MuhametSmaili/nvim) (inspiration
  for Lazy installation of LSP, Autocompletion and Formatter)
  - [0 to LSP : Neovim RC From Scratch](https://www.youtube.com/watch?v=w7i4amO_zaE)
  (smooth vanilla NeoVim tutorial based on `packer`; inspiration for settings/keybindings)
  - [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim)
- tested on macOS (aarch) and Alpine Linux (via [macpine](https://github.com/beringresearch/macpine))
  using NeoVim 0.11, tmux & [ghostty](https://ghostty.org/)

NOTE: I'm new to NeoVim (June 2023; I use it before but Vim 7+ was good enough).
I decided to start from *scratch*. Still have some functions to convert from
VimScript to Lua/Fennel.

## Zsh (my shell)

Because the best shell make the most beautiful pearls. Zsh (by default on
macOS) is a better Bourne and as an interactive shell, it has some good tricks
compared to Bash (even if I may recommend to use Bash for pure scripting when
you have no alternative or a specific environment (Nix, GNU coreutils vs BSD vs
`busybox`,...)).

`ZDOTDIR` should be `$HOME/.zsh`. In this setup, **first run**
`$ZDOTDIR/update_completions.zsh`; it will install completions for:

- Nix (`nix-env`...)
- `zig`
- conditionally:
  - `rustup` & `cargo` if `rustup` is installed
  - NixOS tools (`nix-os-generate-config`...) if you are on NixOS (this might
  change in the future and it may be available to any Linux by default)

For example, some functions are useful to navigate in directories:

- `<CTRL-u>` : go **up** in directory;
- `<CTRL-p>` : go to the **previous** visited directory;

are great bindings to change directory without touching your command line (and so letting ugly `cd ..` in your history).

If you're lost using `zsh`, `bindkey` shows you what you may need.

If `fzf` is normally installed, try:
- `<ALT-c>`  : `fzf-cd-widget` (ie navigate in the subdirectories without touching the command line;
  *memo*: **alt**ernative `cd`)
- `<CTRL-t>` : `fzf-file-widget` (output the files/subdirectories selection)
- `<CTRL-r>` : `fzf-history-widget` (output the shell history selection)

With your `fzf` install, you should have the file `~/.fzf.zsh`; it can let you override
the default settings written in `~/.zsh/zshrc.d/20-environment.zsh`
(here, `fd` & `bat` are required). Here's some useful `fzf`-based functions (or *aliases*):
- `ff` : go to a subdirectory of your projects' root (set `DEVELOPER_ROOT` in `~/.zsh/.zshrc`;
`~/Documents/Code` by default)
- `vv` : select a file to open in `nvim` (also, `vimgrep` using `rg` to open all the matching
files in `nvim`; eg. `vimgrep nvim ~/.zsh`)

Check the configuration of the `zsh` auto-completion in `~/.zsh/zshrc.d/80-complete.zsh`.
Since the end of 2024, it's a work in progress to upgrade some very old functions. For
now, the key bindings are:
- *normal way* (by default using `<TAB>`, there's no menu *ie* `NO_ALWAYS_LAST_PROMPT`):
  - `<TAB>`    : next completion
  - `<SHIFT-TAB>` : previous completion (can be used directly if you expect a better result)
  - `<SPACE>`  : validate
  - `<CTRL-C>` : back
- *menu way*:
  - `<CTRL-n>` (instead of `<TAB>`) : activate the **menu** (you can now select and **multi-select**)
    - in the menu, you can type:
      - arrow keys or vi motion keys (`h`, `j`...)
      - same keys as in the *normal way* (`<TAB>`, `<SHIFT-TAB>`...)
      - `<SPACE>` : ok too
      - `<RET>` : ok **AND stay in the menu** (very cool as a file finder)
      - `<CTRL-P>` (or `<BACKSPACE>`) : undo (eg. you want the word but you mistyped `<RET>`
      instead of `<SPACE>`; `<CTRL-P>` then `<SPACE>` is what you need)
- (experimental; requires the old [matcher](https://github.com/burke/matcher))
  - `<CTRL-X><CTRL-N>` : matches files with *30 characters max* inside `git ls-files` if possible;
  it's **menu-based** by default to be able to multi-select
  - (optional) if `activate_simple_matcher` is set:
    - `<CTRL-X><CTRL-T>` : same matcher but without menu (the lack of multi-select makes this option
    unneeded)
- `<CTRL-X>X` : complete the zsh *aliases*

From `$ZDOTDIR`, the starting point is the `.zshenv` then, during an
interactive login startup, visit:

- `.zprofile` (ensure `/etc/zprofile` has been loaded **ONCE & ONLY ONCE** by
restoring the current path captured by `.zshenv` if the instance is interactive
AND the `.zprofile` has been sourced; don't touch the `__ZPROFILE_SOURCED`
variable)
  - this provides a way to deal with inner `zsh` with a `--login` option
  enabled in the context of macOS or any environment with `path_helper`s or
  builder that may redo the `PATH`
- `.zshrc`
  - adds functions from `zshrc.d/functions`
    - you can add your own in `zshrc.d/functions/custom`
    - `zshrc.d/functions/dev` contains functions autoloaded for the initial
    script; they won't be autoloaded in any session
  - builds the `PATH` and launches environment scripts (ex: `direnv`,
  `sdkman`...) **ONLY ONCE**
    - `zshrc.local_path` will prepend the `PATH` with additional paths from
    functions contained inside the `paths` subdirectory
      - the lower the prefixed numbers the earlier in the `PATH`
    - for this step, it's important that every functions has been loaded before
    building the `PATH` from `.zshrc`
    - `/etc/zshrc` may load `nix-daemon.sh`; it will be run once has the `PATH`
    generator scripts in `.zshrc`
    - not only the local `PATH` is built here but some environment variables
    generated from some vendor scripts as `GOPATH`, `LUAPATH`...
  - sets the environment (variables, alias, prompts...) from the scripts in
  `.zshrc.d`
    - the lower the prefixed numbers the earlier the environment files will be
    sourced
- `.zlogin` manages the `tmux` reattachment when quitting

If `zsh` is loaded in non-login interactive instance, the `.zprofile`/`.zlogin`
files won't be read. If you normally run a subshell, only the `.zshenv` will be
loaded (an additional `__PATH` will be generated as a copy of the actual
`PATH`; don't remove it; it might be used by `.zprofile` to avoid the
`/etc/zprofile` to override the `PATH` with some already-existing global
paths).

Remember that the numbers in front of file names in `$ZDOTDIR/paths` &
`$ZDOTDIR/zshrc.d` show you the loading **order** like on a lot GNU Debian
classical library setup in `/etc`.

`tmux` must be installed in order to use
`$ZODDIR/zshrc.d/99-tmux-sessions.zsh`. On macOS, install the following package
(see
[tmux-MacOSX-pasteboard](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard)):

```sh
brew install reattach-to-user-namespace
```

**IMPORTANT**: some useful functions require `fzf` and `fd`

## Tmux (my terminal manager)

- ensure the version (`tmux -V`) is 3.1+
- this setup requires `fzf`, `bat` and `git`*(!)*; on macOS, you need
  `reattach-to-user-namespace` too (as mentionned at the end of the
  [zsh](#zsh-my-shell) section)
- `fn a` is the prefix when skhd is activated on macOS; `F5` otherwise.
  I use `<C-a>` and `<C-e>` to navigate in a terminal (move cursor at the
  beginning and end of the current line respectively). `<C-b>` was the default
  but it is in conflict with the *snippet backward* default keybinding in
  (Neo)Vim
- the TPM `SessionX` is ready to install (using `F5`+`I`); use `F5`+`o` to
  open it; read the
[documentation](https://github.com/omerxx/tmux-sessionx?tab=readme-ov-file#working-with-sessionx-)

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

All scripts in `.emacs.d/2013-scripts` are dead and tested on Emacs 24.3 only.
You can access to this archive from this  
[revision](https://github.com/martialboniou/dotfiles/tree/db66a2a41cd4fc950a378088de126f5ffef67a19).

## Bat

- add a `~/.config/bat/syntaxes/Fennel.sublime-syntax` for
  [fennel](https://fennel-lang.org) code's syntax highlighting
  in [bat](https://github.com/sharkdp/bat}
- you MUST execute `~/.config/bat/BUILD.sh` to refresh the cache 
