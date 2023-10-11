NeoVim
======

Pre-configuration
-----------------

Ensure `rg` is installed:

```sh
brew install ripgrep
```

Also install a [Nerd version](https://www.nerdfonts.com/) of your favorite
monospaced font for your terminal (I'm happy with Inconsolata), otherwise
some emoticons won't be displayed at all.

Vim keybinding reminders & tips
-------------------------------

- `"+p`  : paste the system clipboard register
- ```0`` : go to the position before exit 
- `<C-6>`/`<C-^>`: switch to the **previously edited file** (**IMPORTANT**:
  use `<C-6>`, not `<C-^>`, on a layout with *dead-keys*; I use an
  international Dvorak layout (+ Command Qwerty) on macOS)
- `:cd` : change the working directory (good for Harpoon; NOTE: `<leader>cd`
  is available to change the working directory **for the current window**
  with `lcd`)
- `gv`   : recall the latest visual block (say, use `<C-v>` to create one):
  - `I`  : insert in visual block
  - `A`  : append in visual block (use `$` to reach the EOL of each line of
           the block; useful to complete lines with commas or semi-colons)
- `gf`   : go to file when cursor is above a filename/hyperlink
- `:map` : check the key mapping
- `<C-f>` : **command mode** after a `:`; behave like any Vim buffer
- navigation with some `z` commands when *wrap* mode is off (default in this setup):
  - `zz`  : center cursor position vertically (**BEWARE**: `ZZ` means save and close buffer; works
            the same when *wrap* mode is on)
  - `z;`  : center cursor position horizontally (added; equivalent to `zszH`; see `remap.fnl`!)
  - `zs`  : scroll horizontally to position the **cursor at the start** of the
            screen
- `G`    : go to the end of file (of course!)
- `J` (in visual) : move down the visual block (added; see `remap.fnl`!)
- `<C-j>` : go down in the quickfix list (see below)
- `:copen` : open the quickfix list (say, after a `:grep foo src/*`)
- `V:s/foo/bar/g<CR>` : replace *foo* by *bar* in the selection
- `:%s/\(.\)noremap(/vim.keymap.set("\1", <CR>` : replace an old
  `nnoremap` function in standard vim Lua (nice trick!)
- Netrw specific:
  - `%` : create file
  - `d` : create directory
  - `gn` : change the current directory to the folder under the cursor
  - `gh` : show/hide hidden files

Plugins and new keybindings
---------------------------

### Reminders

- **IMPORTANT!**: Lazy is the unique package manager
- the `<leader>` key is `<Space>` in this configuration
- the `<localleader>` key is `,` in this configuration
- `<C-c>` as `<Esc>` (choice made by ThePrimeagen from IntelliJ IDEA)
- Harpoon is *Cwd*-dependent; ensure you start NeoVim at the root of
  your current project (notice you can harpoon a file under the cursor
  in a Netrw or `mini.files` buffer)
- (**MINOR BUG**): the [rainbow delimiters](https://github.com/HiPhish/rainbow-delimiters.nvim)
  plugin (used to highlight the parentheses) stops when you format
  (with `<leader>f`); edit new content will fix this issue

### About Netrw

It's great but:
- use `mini.files`:
  - IMPORTANT: `h`/`l` : navigate up/down (`l` on a file = load in buffer)
  - `g?` : help
  - `@`  : reveal `cwd`
  - `=`  : synchronize (IMPORTANT: not :wq)
  - `gh` : show/hide hidden files
  - edition like `vim-vinegar`/`oil`:
    - btw, `-` is important in a buffer, DON'T TOUCH IT
  - BAD IDEA: don't add `-` as alias of `h` in the `mini.files` buffer
  - Netrw still available: `:Ex` (or open a directory `nvim .`)
- I CANNOT switch to `oil.nvim`
  - `-` in normal mode : open folder directory in `oil.nvim` (like `vim-vinegar`)
  - `<leader><leader>`/`<leader>pv` : `mini.files` not `oil.nvim` (instead of `:Ex`)
  - it **BREAKS** Netrw
- Telescope is good enough and faster than FZF
- `:Ex` is still available

### Example of workflow

- (locally) `<leader>cd` : change locally the working directory
- `<leader>a` : tag a file in harpoon (first)
- `<leader><leader>` or `<leader>pv` : project view at the current directory
  (the cursor is on the current file AKA `:pwd`) or at the current working
  directory (root of the project AKA `:lua print(vim.loop.cwd())`)
  respectively
- make a file, say, a module
  - edit the current buffer from `<leader><leader>`
  - `=` to synchronize
  - (optional) otherwise: `%` if you used `:Ex`
- `<C-h>` : back to first file
- (optional) `<C-Space>` : enable completion
- (optional) `<C-y>` : auto-complete a path reference to the newly created module (<C-n>/<C-p> to navigate)
- `gd` (on a reference, say, `require`; normal mode) : back to the module
- `<leader>a` : tag the module in harpoon (second)
- `<leader>e` : check harpoon (change the order with copy-pasta; this was <C-e> is the original configuration)
- `<C-h>`/`<C-t>` : switch back and forth, the file and its module

### Cheat sheet

- `<C-o><cmd>` (in insert): switch to normal to execute `<cmd>` then back to 
insert
- `J` : append line (in normal) BUT in this remap, it doesn't move the cursor away
- `<C-d>`/`<C-u>` : page up/down (doesn't move the cursor in this remap; 
  *memo*: Down/Up)
- `:so` : source this file
- `<C-w><C-w>` : cycle windows
- `<C-w>o` : one window (AKA 'close' others)
- `<leader>cd` : change the working directory for the current window
- in *visual* mode:
  - `<shift-J>`: move down the whole selection
  - `<shift-K>`: move up the whole selection
- *REMINDER*: `:map` to check the key mapping
- `:Lazy` : check package (I prefer this one to Packer)
- `:TSPlaygroundToggle` : display the AST (neat!)
- **IMPORTANT!**: `<leader>s` : create a template to replace the current word (*memo*: `s` as in `:%s`)
- `<leader>s` (in visual) : create a template to replace a pattern in the selection (added by me)
- `<leader><leader>` : `mini.files` at current directory; or `:Ex` (faster than `<leader>pv`)
- **BEWARE:** `<leader>p` (in selection) : paste a buffer but doesn't keep the deleted selection so you can paste the same again
- `<leader>pv` : `mini.files` at root (ie *Cwd*)
- `<leader>pf` : telescope find files (*memo*: project files)
- `<leader>ps` : telescope project search (`rg`!)
- `<leader>vh` : telescope view helptags
- `<leader>vv` : telescope recent files (*memo*: view viewed/view visited);
  `<leader>vr` is deprecated (poor ergonomics); not in the ThePrimeagen setup
- `<leader>vb` : telescope buffers (*memo*: view buffers); not in the
  ThePrimeagen setup
- `<leader>gs` : git status (you can lazy load the FuGITive plugin with the
  command `Git`)
- `<leader>a`  : add file in harpoon
- `<leader>e`  : harpoon quick menu (it was `<C-e>`)
- `<leader>u`  : undotree
- **VERY IMPORTANT**: `<leader>y`/`<leader>Y`/`<leader>d` : yank or delete for the clipboard
- `<leader>x`  : toggle the executability of a file (different from the ThePrimeagen's version)
- `<leader>j`/`<leader>k` : quickfix local navigation (ie `lnext`/`lprev`; inverted from ThePrimeagen's)
- `<C-j>`/`<C-k>` : quickfix navigation (ie `cnext`/`cprev`; inverted from ThePrimeagen's;
  more natural; same order as in quickfix/**Trouble**)
- **IMPORTANT (normal mode)**: `<C-p>` : telescope git files (*memo*: control project)
- `<C-h>` (also `t`,`n`,`s`: dvorak!) : navigate file 1 (2,3,4) in harpoon
- `<C-f>`      : navigate thru tmux sessions (this executable (file)[https://github.com/ThePrimeagen/.dotfiles/blob/master/bin/.local/scripts/tmux-sessionizer] is required in your path)
- **IMPORTANT**: `Q` is removed (in normal mode; avoid typo)
- FuGITive (inside the fugitive buffer only):
  - `<leader>p` : `git push`
  - `<leader>P` : `git pull` **with rebase** 
  - `<leader>t` : `git push -u origin` template; complete with the branch name
    to push to
- LSP case:
  - *cmp_mappings*:
    - `<C-y>` : confirm completion
    - `<C-p>` : previous completion
    - `<C-n>` : next completion
    - `<C-Space>` : complete
    - `<C-f>` : snippet forward selection (default by LSP Zero; navigate thru tmux sessions in normal mode)
    - `<C-b>` : snippet backward selection (default by LSP Zero; unbind `<C-b>` for `tmux`;
      don't use `<C-a>` either because you lose the cursor navigation (start of line);
      `F5`/`fn a` is my current choice for `tmux`)
    - other keys (imposed by vim and added by LSP Zero)
      - `<C-e>` : cancel the completion
      - `<C-u>`/`<C-d>` : scroll the document up/down
   - in LSP buffer only (normal mode except when said otherwise)
     - `<leader>f`  : format LSP
     - `<C-h>` (insert mode) : signature (*BEWARE*: `<C-h>` switches to the
        first harpoon in **normal mode**)
     - `K` : hover (*BEWARE*: `K` moves the selection up in **visual mode**)
     - `gd` : goto definitions (**IMPORTANT**: jump to the file in LSP-injected files; say, like lua vim configurations)
     - `<leader>vws` : view workspace symbol
     - `<leader>vd` : view diagnostic
     - `[d` : next diagnostic
     - `]d` : previous diagnostic
     - these following keybindings come with a shorter version for ergonomics:
       - `<leader>vca`/`<leader>ca` : **v**iew **c**ode **a**ction OR simply, **code action**
       - `<leader>vrr`/`<leader>rr` : **v**iew **r**efe**r**ences OR simply, **RefeRences**
         (*BEWARE*: `<leader>r` + another key is used by the *refactoring* plugin; see below)
       - `<leader>vrn`/`<leader>nn` : **v**iew **r**e**n**ame OR simply, **new name**
- Primeagen's refactoring plugin (inspired by Martin Fowler):
  - use `:Refactor e<Tab>` or one of these keybindings (in **selection mode**
    by default):
    - `<leader>re` : extract function
    - `<leader>rf` : extract function to a file
    - `<leader>rv` : extract variable (extract occurences of a selected expression
    to its own variable, replacing occurences of that expression with the variable)
    - `<leader>ri` (also in **normal mode**) : inline variable (opposite of
    extract variable; replace all occurences of a variable with its value)
    - `<leader>rb` (**only in normal mode**) : extract block
    - `<leader>rbf` / `<leader>rbb` (**only in normal mode**) : extract block
      to a file
- **Trouble** (fix helper plugin) case:
  - `<leader>xx` : toggle trouble quickfix (*memo*: quickfiXX)
- Github Copilot is ready to use. You must `:Copilot auth` the very first time.
  If Copilot is not loaded, type `:Copilot` or start asking for a suggestion
  with `<C-]>`. Copilot is not auto-triggered by default; type `:CopilotTrigger`
  to do so (NOTE: if the auto-completion menu disturbs you, use `<C-e>` to disable
  the menu and `<C-Space>`, after that, to restore it).  These following
  keybindings are available in **insert mode**:
  - `<M-]>` : next suggestion/start the suggestion (*lazy-load* if not loaded)
  - `<M-[>` : previous suggestion
  - `<C-]>` : accept next word from the suggestion
  - `<C-[>` : cancel the suggestion
  - `<M-=>` : accept the suggestion (**IMPORTANT!**: the default keybinding
    `<M-l>` was reserved in my [yabai](https://github.com/koekeishiya/yabai)
    setup; *memo*: `=` to synchronize as already used in `mini.files`; also,
    `=` is just below `]` on the dvorak layout)
- [Cellular Automaton](https://github.com/Eandrju/cellular-automaton.nvim)
  (setup in `plugins/procrastinate.lua`)
  - `<leader>z`  : randomly activate an animation

### Technical tips

#### Language Server Protocol

About the lazy loading of `nvim-lspconfig`, `nvim-cmp` & `null-ls` (without LSP Zero):
- LSP
  - neovim/nvim-lspconfig
    - event: BufReadPost
    - dependencies:
      - williamboman/mason.nvim
        - run: `:MasonUpdate`
      - hrsh7th/nvim-cmp (see Autocompletion)
      - (optional) rrethy/vim-illuminate (highlight same word)
      - (optional) glepnir/lspaga.nvim (fancy navbar)
    - opts:
      - diagnostics
      - autoformat
      - servers
    - config:
      - requirements
        - cmp_nvim_lsp
          - `default_capabilities(vim.lsp.protocol.make_client_capabilities())` 
        - mason-lspconfig
          - `setup({ ensure_installed = ... })`
          - `setup_handlers({ function(server_name) ... end })`
        - lspconfig
- Autocompletion (CMP)
  - hrsh7th/nvim-cmp
    - dependencies:
      - hrsh7th/cmp-nvim-lsp
      - L3MON4D3/LuaSnip
        - build: `make install_jsregexp`
      - (optional) rafamadriz/friendly-snippets
      - saadparwaiz1/cmp_luasnip
      - (optional) hrsh7th/cmp-buffer (buffer words)
      - (optional) hrsh7th/cmp-path (filesystem paths)
      - (optional) ray-x/lsp_signature.nvim (show function signatures)
      - (replaced by previous) hrsh7th/cmp-nvim-lsp-signature-help
- `null-ls` (Additional Formatters, Diagnostic tools and Spellchecking)
  - jay-babu/mason-null-ls
    - depe
  - jose-elias-alvarez/null-ls.nvim (frozen?)
    - event: BufReadPost
    - config:
      - requirements
        - luasnip.loaders.from_vscode
          - `lazy_load()`

#### Fennel as the main programming language

Fennel code is used to set up this NeoVim. No worries: you still can add your
own plugins written in Lua in `lua/hondana-dev/plugins/unchecked`. Here's the
form of these plugins spec file content:

```lua
-- ~/.config/nvim/lua/hondana-dev/plugins/unchecked/<project-name>.lua
return {
    "<account-name>/<project-name>", -- from github.com
}
```

**IMPORTANT**: If you put your plugins in `lua/hondana-dev/plugins`, please
choose an original filename (say, add your name as a  prefix). If another
plugin spec file with the same name may exist in `fnl/hondana-dev/plugins` 
in the future, the compiled version in `lua/hondana-dev/plugins` would crush
yours.

If you want to edit some Fennel code, notice that:
- the Fennel Language Server should work for diagnostics
- the `<leader>f` keybinding for formatting (ie. `vim.lsp.buf.format()`) will
  work if you have [fnlfmt](https://git.sr.ht/~technomancy/fnlfmt) in your
  `$PATH` (the snippet `;skip` prints a special comment if you want to
  locally skip the formatting)

Here are some keybindings for the Fennel buffer (mainly to access a REPL):
- from the [Tangerine](https://github.com/udayvir-singh/tangerine.nvim) plugin
  (NOTE: `<leader>g` is a prefix for git-related actions (as `<leader>gs` and
  other FuGITive commands); here, `g` followed by a **capital letter** is
  the pattern.):
  - `gE` : **E**valuate
  - `gL` : **L**ua output
  - `gC` : **C**ompile the file in the current active Fennel buffer into a Lua
    file (this keybinding is not available in the default configuration; don't
    forget to save the file as the compiler doesn't use the current buffer
    itself)
  - `gO` : **O**utput the Lua file (**IMPORTANT**: your Fennel files must be in
    a `fnl/` directory so your compiled Lua files will be pushed into a
    `lua/`-rooted directory tree; to move back to the Fennel buffer, remember
    `<C-6>` is your friend; also, don't forget to `gC`/`:FnlCompileBuffer`
    before `gO`)
  - `<C-c>` (in the float output buffer) : kill (instead of `<Esc>`) 
- from the [Conjure](https://github.com/Olical/conjure) plugin (NOTE: every
  evaluation is stored in a register, try `"cp`):
  - evaluate: 
    - `<localleader>eb` : **e**valuate the whole **b**uffer
    - `<localleader>ee` : **e**valuate the inn**e**r form
    - `<localleader>er` : **e**valuate the oute**r** form
    - `<localleader>e!` : **e**valuate a form and replace it with the result
    - `<localleader>em<letter>` : **e**valuate a form at the `<letter>` mark
      (created with `m<letter>`)
  - inspect:
    - `<localleader>ew` : inspect by **e**valuating *w*hat it is
    - `<localleader>E` (in visual mode) : inspect by **E**valuating the selection
    - `<localleader>Eiw` (in normal mode) : inspect by **E**valuating the inner word
    - `<localleader>Ea(` (in normal mode) : inspect by **E**valuating the whole parens (`a(`)
  - log buffer:
    - `<localleader>ls` : open the **l**og buffer horizontally **s**plit
    - `<localleader>q`  : close any log buffer (*ie* **q**uit)
    - `<localleader>lr` : soft **l**og **r**eset (leaving the window open)
    - `<localleader>lR` : hard **l**og **R**eset (closing the window open, deleting the buffer)
  - doc word:
    - `<localleader>K`  : doc word (instead of `K`; as used in LSP for hover)

Here's some tips for the LISP typists:
- (insert mode) `,\<Space>` : print `λ` (only in this configuration; a viable
  **keyword** in Fennel)
- (insert mode) <C-k> + `*` + `l`: print `λ` (same as above but this is an 
  universal keybinding for Vim/NeoVim)

### Note about specific environment for developers

#### Clang (C/C++)

This setup is not ready for Clang with LSP; you'll have to install `clangd`
via `Mason` (type `i` when the cursor is above `clangd`).

Additionally, configure your project by setting a `compile_flags.txt` at
the root of your project like this (change the path according to your
install):

```
-Wall
-Wextra
-I/opt/homebrew/include
```

Report to the [documentation](https://clangd.llvm.org/config) for other
settings (say, if you use `cmake`; I generally build by `zig` or other
minimalist scripts).

The default formatting is *annoying* (set to 2 spaces' indent). You should
add `.clang-format` at the root of your project too:

```yaml
BasedOnStyle: LLVM
IndentWith: 4
```

#### Symfony 6 (PHP)

If you want to use this install to make NeoVim as
a *ready-to-go* Symfony editor (and get rid of VSCode), do it so.

First, install with `:Mason` (type `i` when the cursor is above
one of the following package):

- `php-cs-fixer` (not needed if already in your path): LSP has already been
extended by [null-ls.nvim](https://github.com/jose-elias-alvarez/null-ls.nvim)
and the `php-cs-fixer` script is ready to use with the `vim.lsp.buf.format()`
command (`<Space>f` is the keybinding for the code formatting)
- `php-actor` (need a restart): `phpactor.json` auto-created at the root
of the project (start NeoVim there!) and auto-configured after Symfony
was detected. It'll make the editing smooth (autocompletion, auto-imports,
...)

(Optional) For the [Twig](https://twig.symfony.com) template engine:
- `twigcs` diagnostics:
```shell
composer global require friendsoftwig/twigcs
```
- [djLint](https://www.djlint.com/docs/getting-started/) formatter (Python3 required;
NOTE: this is the best way to format Twig properly as the
[twig melody](https://github.com/trivago/prettier-plugin-twig-melody) plugin
is not compatible with any recent `prettier` versions that `null-ls` may
rely on):
```shell
pip3 install djlint
```

### Note about the *emoji* completion

The plugin `cmp-emoji` has been added. Type `:` to open the completion menu
anywhere in a **markdown** file or a `git commit` message. This completion is
disabled for other buffers but in comments or strings (ensure there's a space
after the opening `"` before typing the `:`). NOTE: The emoji's completion is
disabled for strings in Fennel code because TreeSitter sees symbols (starting
with colon) as strings.
