{1 :nvim-telescope/telescope.nvim
 :dependencies [[:nvim-lua/plenary.nvim]]
 :tag :0.1.2
 :keys [;;; PROJECT
        ;;  -> files (memo: project files)
        {1 :<leader>pf
         2 #(#($.find_files) (require :telescope.builtin))
         :desc "Open a fuzzy file picker"}
        ;;  -> rg-filtered files (memo: project search)
        {1 :<leader>ps
         2 #(#($.grep_string {:search (vim.fn.input "Grep > ")}) (require :telescope.builtin))
         :desc "Open a string-filtered file picker (cwd)"}
        ;;; VIEW
        ;;  -> recent files (memo: view visited; vr = BAD ERGO)
        {1 :<leader>vv
         2 #(#($.oldfiles) (require :telescope.builtin))
         :desc "Open a fuzzy recent file picker"}
        ;;  -> buffers (memo: view buffers)
        {1 :<leader>vb
         2 #(#($.buffers) (require :telescope.builtin))
         :desc "Open a fuzzy buffer picker"}
        ;;  -> helptags (memo: view helptags)
        {1 :<leader>vh
         2 #(#($.help_tags) (require :telescope.builtin))
         :desc "Open a help tag picker"}
        ;;; GIT
        ;;  -> files (memo: control project)
        {1 :<C-p>
         2 #(#($.git_files) (require :telescope.builtin))
         :desc "Open a git repo fuzzy file picker"}]}
