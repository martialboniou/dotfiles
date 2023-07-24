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
- `:map` : check the key mapping
- `G`    : go to the end of file (of course!)
- `J` (in visual) : move down the visual block (added!)
- `V:s/foo/bar/g<CR>` : replace *foo* by *bar* in the selection
- `:%s/\(.\)noremap(/vim.keymap.set("\1", <CR>` : replace an old
  `nnoremap` function in standard vim lua (nice trick!)
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
- `<C-c>` as `<Esc>` (choice made by ThePrimeagen from IntelliJ IDEA)
- Harpoon is *Cwd*-dependent; ensure you start NeoVim at the root of
  your current project (notice you can harpoon a file under the cursor
  in a Netrw or `mini.files` buffer)

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
  memo: Down/Up)
- `:so` : source this file
- `:Lazy` : check package (I prefer this one to Packer)
- `<C-w><C-w>` : cycle windows
- `<C-w>o` : one window (AKA 'close' others)
- `:TSPlaygroundToggle` : display the AST (neat!)
- *REMINDER*: `:map` to check the key mapping
- **IMPORTANT!**: `<leader>s` : create a template to replace the current word (memo: `s` as in `:%s`)
- `<leader>s` (in visual) : create a template to replace a pattern in the selection (added by me)
- `<leader><leader>` : `mini.files` at current directory; or `:Ex` (faster than `<leader>pv`)
- **BEWARE:** `<leader>p` (in selection) : paste a buffer but doesn't keep the deleted selection so you can paste the same again
- `<leader>pv` : `mini.files` at root (ie *Cwd*)
- `<leader>pf` : telescope find files (memo: project files)
- `<leader>ps` : telescope project search (`rg`!)
- `<leader>vh` : telescope view helptags
- `<leader>vv` : telescope recent files (memo: view viewed/view visited);
  `<leader>vr` is deprecated (poor ergonomics); not in the ThePrimeagen setup
- `<leader>vb` : telescope buffers (memo: view buffers); not in the
  ThePrimeagen setup
- `<leader>gs` : git status (you can lazy load the FuGITive plugin with the
  command `Git`)
- `<leader>a`  : add file in harpoon
- `<leader>e`  : harpoon quick menu (it was `<C-e>`)
- `<leader>u`  : undotree
- **VERY IMPORTANT**: `<leader>y`/`<leader>Y`/`<leader>d` : yank or delete for the clipboard
- `<leader>k`/`<leader>j`/`<C-k>`/`<C-j>` : quickfix navigations (TODO: test this)
- `<leader>x`  : toggle the executability of a file (different than the ThePrimeagen's version?)
- **IMPORTANT**: `Q` is removed (in normal mode; avoid typo)
- **IMPORTANT (normal mode)**: `<C-p>` : telescope git files (memo: control project)
- `<C-h>` (also `t`,`n`,`s`: dvorak!) : navigate file 1 (2,3,4) in harpoon
- `<C-f>`      : navigate thru tmux sessions (this executable (file)[https://github.com/ThePrimeagen/.dotfiles/blob/master/bin/.local/scripts/tmux-sessionizer] is required in your path)
- in *visual* mode:
  - `<shift-J>`: move down the whole selection
  - `<shift-K>`: move up the whole selection
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
    - `<C-b>` : snippet backward selection (default by LSP Zero; unbind `<C-b>` for `tmux`; don't use `<C-a>` because you lose the cursor navigation (start of line);
    `<C-q>` is a good candidate for the `tmux` prefix if you don't use emacs;
    `<M-a>` is my current choice for `tmux`)
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
  - `<leader>xx` : toggle trouble quickfix (memo: quickfiXX)
- [Cellular Automaton](https://github.com/Eandrju/cellular-automaton.nvim)
  (setup in `plugins/procrastinate.lua`)
  - `<leader>z`  : randomly activate an animation

### Technical tips

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

### Note for beginners using PHP

I've had developed a lot with the Symfony 5/6 framework. Not as pro as
Spring but it works. If you want to use this install to make NeoVim as
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
after the opening `"` before typing the `:`).
