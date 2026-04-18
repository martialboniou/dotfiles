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

## NeoVim 0.12 (my main editor)

- the version in `config/nvim.obsolete` has been **frozen**
- please check [this one](https://codeberg.org/martialboniou/nvim)

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

### Emacs 30 (used for Common Lisp projects)

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
