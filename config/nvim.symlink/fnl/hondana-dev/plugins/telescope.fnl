(import-macros {: cal!} :hondana-dev.macros)

{1 :nvim-telescope/telescope.nvim
 :dependencies [[:nvim-lua/plenary.nvim]]
 :tag :0.1.8
 :opts #(do
          (vim.cmd "hi! link TelescopeMatching String")
          [])
 :keys (let [tb (λ [f ...] (cal! :telescope.builtin f ...))]
         [;;; PROJECT
          ;;  -> files (memo: project files)
          {1 :<leader>pf 2 #(tb :find_files) :desc "Open a fuzzy file picker"}
          ;;  -> rg-filtered files (memo: project search)
          {1 :<leader>ps
           2 #(tb :grep_string {:search (vim.fn.input "Grep > ")})
           :desc "Open a string-filtered file picker (cwd)"}
          ;;; GIT
          ;;  -> files (memo: control project)
          {1 :<C-p>
           2 #(tb :git_files)
           :desc "Open a git repo fuzzy file picker"}
          ;;; VIEW
          ;;  -> recent files (memo: view visited; vr = BAD ERGO)
          {1 :<leader>vv
           2 #(tb :oldfiles)
           :desc "Open a fuzzy recent file picker"}
          ;;  -> helptags (memo: view helptags)
          {1 :<leader>vh 2 #(tb :help_tags) :desc "Open a help tag picker"}
          ;;  -> buffers (memo: view buffers + bb as alias)
          (-> [:vb :bb]
              (#(icollect [_ key (ipairs $)]
                  {1 (.. :<leader> key)
                   2 #(tb :buffers)
                   :desc "Open a fuzzy buffer picker"}))
              (unpack))])}
