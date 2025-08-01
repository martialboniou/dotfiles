(import-macros {: tc} :hondana-dev.macros)

(macro nav-file-mapping! [...]
  (local out [])
  (each [unit key (ipairs [...])]
    (table.insert out
                  `{1 ,(.. :<C- key ">")
                    2 #(#($:select ,unit) (: (require :harpoon) :list))
                    :desc ,(.. "Go to the #" unit " harpooned file")}))
  out)

(tc return boolean)
(λ netrw-buf? []
  (let [buf-type (vim.api.nvim_buf_get_option 0 :filetype)]
    (= :netrw buf-type)))

(tc return boolean)
(λ minifiles-buf? []
  (let [buf-type (vim.api.nvim_buf_get_option 0 :filetype)
        buf-name (vim.api.nvim_buf_get_name 0)]
    (or (and (= :minifiles buf-type) (= buf-name "")) (= nil buf-name))))

(tc param fpath string return HarpoonItem)
(λ harpoon-path-list [fpath]
  ;; FIXME: use a harpoon core function to build `value`?
  (let [path (require :plenary.path)
        key-fn (or (?. (require :harpoon) :config :settings :key) #(vim.uv.cwd))
        buf-name (path:new fpath)
        context {:row 1 :col 0}
        value (buf-name:make_relative (key-fn))]
    {: context : value}))

(λ harpoon-file-explorer []
  "harpoon files in buffer or selected in an explorer (Netrw/mini.files)"
  (let [hlist (: (require :harpoon) :list)]
    (if (netrw-buf?)
        (do
          (vim.cmd " let netrw#current_word = netrw#Call('NetrwFile', netrw#Call('NetrwGetWord')) ")
          (let [cw (. vim.g "netrw#current_word")]
            (when (= 0 (vim.fn.isdirectory cw))
              (hlist:add (harpoon-path-list cw)))))
        (minifiles-buf?)
        (let [{: get_explorer_state : get_fs_entry} (require :mini.files)
              (ok res) (pcall get_explorer_state)]
          (when (and ok (not= res nil))
            (local fs-entry (get_fs_entry))
            (when (and fs-entry (= :file fs-entry.fs_type))
              (hlist:add (harpoon-path-list fs-entry.path)))))
        (hlist:add))))

(tc type LazySpec)
(local P ;;
       {1 :theprimeagen/harpoon
        :branch :harpoon2
        :dependencies [:nvim-lua/plenary.nvim]
        :keys [{1 :ga
                2 harpoon-file-explorer
                ;;  👍 : error if the cursor in Netrw or mini.files not on a file
                :desc "Harpoon the current file"}
               {1 :gu
                ;; it was <C-e> but <C-a>/<C-e> = cursor navi in terms
                2 #(let [h (require :harpoon)]
                     (h.ui:toggle_quick_menu (h:list)))
                :desc "Toggle the harpoon's quick menu"}
               ;; CTRL + h,t,n,s = keys for Dvorak layout
               ;; (you can add more to access #5+ harpooned files)
               ;; WARN: <C-s> is used by blink.cmp to toggle signature in
               ;; insert mode instead of the default <C-k> used for digraphs
               ;; (see `hondana-dev.plugins.completion`)
               (unpack (nav-file-mapping! :h :t :n :s))]
        :lazy true
        ;; setup as method is MANDATORY
        :config #(let [h (require :harpoon)]
                   (h:setup {:settings {:save_on_toggle true}}))})

P
