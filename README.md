# What?

Some useful dotfiles for developers using applications such as zsh, tmux, vim, emacs... on POSIX systems. Used mainly on macOS and linux.

# For whom?

Me and my roomies. But anyone can find something cool here.

# How?

Use the [easy](https://github.com/holman/dotfiles) way:

    mkdir -p ~/Documents/Code
    git clone git@gitlab.com:hondana-dev/dotfiles
    cd !$/dots && script/bootstrap

# And?

## Config directory

Every files/directories in `config/` will be linked to the `$HOME` directory as a subdirectory of `.config/` instead of having their symlink in the `.<filename>` form.

## NeoVim

- Minimal configuration (keybindings are made for dvorak keyboards)
- [Lazy](https://github.com/folke/lazy.nvim) (`nvim .` should load Netrw and
a colorscheme only; everything loads when needed)
- Telescope (used as fuzzy finder) & [Harpoon](https://github.com/ThePrimeagen/harpoon)
- [mini.files](https://github.com/echasnovski/mini.files) (very good to navigate,
  preview files and manage directories; replace Netrw even if it's still available
  thru `:Ex`)
- Undotree & Fugitive (same in my classic Vim setup)
- [Treesitter](https://github.com/nvim-treesitter/nvim-treesitter) (interface
  to a fast parser generator tool and incremental parsing library)
- Mason-boosted LSP & CMP (no [zero](https://github.com/VonHeikemen/lsp-zero.nvim))
  (work in progress!)
- others:
  - [autopairs](https://github.com/windwp/nvim-autopairs) (disabled in Telescope
  & mini.files)
- sources:
  - [good lazy configuration](https://github.com/MuhametSmaili/nvim) (inspiration
  for Lazy installation of LSP, Autocompletion and Formatter)
  - [0 to LSP : Neovim RC From Scratch](https://www.youtube.com/watch?v=w7i4amO_zaE)
  (smooth vanilla NeoVim tutorial based on `packer`; inspiration for settings/keybindings)
- tested on macOS (aarch) using NeoVim 0.9, tmux & iTerm2 (should work with 0.8)

NOTE: I'm new to NeoVim (June 2023; I use it before but Vim 7+ was good enough).
I decided to start from *scratch*. Still have some functions to convert from
VimScript to Lua.

## Zsh

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

## Vim 8

The packages will be auto-installed via vim-plug. Untested on NeoVim. Install FZF and yarn if possible.

## GNU Emacs (working in progress)

All scripts in `.emacs.d` are currently tested on Emacs 24.3. **Ensure your LOAD-PATH know this path:** `~/.emacs.d/lisp` to be able to launch partial setup (see my `zsh` setup).

The first Emacs is launched, Emacs asks at startup about:

-   your environment of typing: _Vim-like_ or _Emacs_. If _Vim-like_ is emulated, _Evil_ is used by default and the other tools are customized to work with this environment;
-   your keyboard: _Dvorak_ or _Qwerty_. If _Dvorak_ is chosen, a new keybinding appears for copy/paste. This keybinding is based upon the Oracle/Sun 5/6 keyboards' extra keys' order (on the left part of the keyboard): `C-; C-;` stands for `kill-region` (`CUT`), `C-; C-a` stands for `yank` (`PASTE`) and eventually `C-; C-'` stands for `copy-region-as-kill` (`COPY`);
-   your use of `C-w` and `C-h` for deletion as on ANSI terminals: if true, those keybindings behave for delete a word and a character, respectively. The `C-w` keybinding is normally used to cut a region: alternatively, a _Vim-like_ will use the _Vi_ keybindings in _Evil_ normal mode; a `CUA` user will use the delayed `C-x` keybinding; a _Dvorak_ typist will use the new `C-; C-;` keybinding; and others would like to create their own keybindings (by default `C-x 6 ;` should be available);
-   your need of packages: _Full install_ or _None_. If you want the complete installation, the system will install the recommended packages to optimize the use of this setup. Otherwise, no `el-get` packages will be installed (useful for custom/testing/offline session). If you want a _Vim-like_ environement at startup and _None_ of the recommended packages, be careful that you will need to `M-x el-get-install` the package named `evil`.

Once your Emacs is launched, a file `.emacs.d/data/.launched` is created to register your choice. If you want to change, you may delete it. If you want another setup for one specific session, you may force _Emacs_ to be launched with other options using:

-   `*vim-now*` boolean to enable/disable the _Vim-like_ setup;
-   `*dvorak-now*` boolean to enable/disable the _Dvorak_ keyboard setup;
-   `*term-now*` boolean to enable/disable the `C-w` & `C-h` as deletion keybindings;
-   `*full-ammo-now*` boolean to enable/disable the full `el-get-sources` install.

Those forcing options may be set at the heading of the `init.el` file in the `.emacs.d/` directory by using `defvar`.

A minimal setup may be launched any time by running explicitly the `kernel` script (Emacs will detect an abnormal startup and won't require additional setup):

    $ emacs -q -l kernel

Other partial setup may be launched by this method: `mail` will load every `org` and `wl` (wanderlust) configuration files, `peyton-jones-family` and `rectify` together should run a good environment uniquely enhanced to develop programs based on _ML_ with snippets and flymake shortcuts... You also may force options in command line. Say, we want _minimal setup with Vim-like environement_:

    $ emacs -q -e "(defvar *vim-now* t)" -l kernel

It works even if your initial statement was not to let `vim` emulation to run. A simpler approach would be to load `vim-everywhere`:

    $ emacs -q -l vim-everywhere

The following list is some of my recommended `el-get` packages (or, if unreachable, those provided by programs in `vendor`directory or better, in the `emacs-revival` archive):

-   `evil` and `undo-tree`
-   `ido`
-   `org-agenda` on `org-mode` 7+
-   `remember`
-   `wanderlust`
-   `wdired` (an _editable_ `dired`; some wrappers in `lisp` for `viper` or `evil` case)
-   `anything`
-   `ibuffer` (a `dired` for buffers)
-   `yasnippet`
-   `auto-complete`
-   `hippie-expand` (on `C-p`)

Some packages may also be useful in order to help this configuration to work fine. For example:

-   `tiling`: to split and reorganise windows;
-   `revive`: to save and restore multiple window configurations;
-   `emms`: to transform emacs in media player;
-   `pp-c-l`: to print `^L` as a page separator;
-   `newsticker`: to read RSS (not optimal for instance);
-   `multi-term`: to manage multiple `term` windows on POSIX systems.

`anything` is not as fast/smart as `ido` but it's very useful for displaying a one shot buffer. For example, type `<f5><f8>` to display a list including:

-   current buffers;
-   recently open files;
-   all files and directory in the current.

See `.emacs.d/lisp/shortcuts.el` for good ideas of bindings especially for buffer/window/frame navigation using tiling/cycling (thing about larswm or [Xmonad](http://xmonad.org/tour.html)) or for `<f5>`-`<f8>` keys (`<f5><f5>` toggle the current frame from multiple buffers to a single view on the current buffer via `revive.el`).

Read `.emacs.d/lisp/vars.el` to configure your path. Remember this! By default, `.emacs` makes Emacs to:

1. support UTF-8 (`.emacs.d/lisp/formats.el` to switch to another encoding) / remove eye candies (no toolbar / no scrollbar);
2. create `load-path` (all files and _subdirs_ from `.emacs.d/vendor`);
3. load uninstalled packages via scripts in `lisp/packs.el`;
4. generate `autoloads` at the root of `.emacs.d/vendor` (loading `cedet` if there's extended autoloads for `eieio` classes and methods);
5. add handmade autoloads if needed;
6. configure general behavior:

-   buffers and `ido` (**BEWARE**: `C-x C-b` to switch buffers, `C-x C-f` to find file in history, `C-x f` to find file normally),
-   `desktop` + `revive` (to save window configuration on quit),
-   `autosave` and `auto-save-list` (no more `*~` everywhere; all temporary files are kept in `.emacs.d/data`)

7. load specific configuration files.

Here, `require` is often avoided because automagically generated `autoload`s and `eval-after-load` is over-used. (That's why Emacs should start quickly. Don't worry if your first startup is slow: Emacs will fetch additional packages like `nxhtml`, a newest `cedet` or `wanderlust` in order to work ultimately.)

`.emacs.d/lisp/vim-everywhere` is a bit special and hard to understand. It is here to:

-   load `evil-mode`
-   extend the fringe to display line numbering
-   extend colors with `font-lock-number-face` and `hl-line+`
-   colorize keywords like **FIXME:**... and global line `<.>` viper state
-   grab your current `colorscheme` in your `.vimrc` and use the same in `emacs` if any (a 256 colors compatible colorscheme adaptation named `wombat256mod` is displayed by default)
-   add bindings like `M-0` to open the current buffer in Vim
