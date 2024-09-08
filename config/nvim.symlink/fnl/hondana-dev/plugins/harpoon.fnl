(macro nav-file-mapping! [...]
  (local out [])
  (each [unit key (ipairs [...])]
    (table.insert out
                  `{1 ,(.. :<C- key ">")
                    2 #(#($:select ,unit) (: (require :harpoon) :list))
                    :desc ,(.. "Go to the #" unit " harpooned file")}))
  out)

(λ netrw-buf? []
  (let [buf-type (vim.api.nvim_buf_get_option 0 :filetype)]
    (= :netrw buf-type)))

(λ minifiles-buf? []
  (let [buf-type (vim.api.nvim_buf_get_option 0 :filetype)
        buf-name (vim.api.nvim_buf_get_name 0)]
    (or (and (= :minifiles buf-type) (= buf-name "")) (= nil buf-name))))

;; with harpoon 1, I was able to attach a file from Netrw or `mini.files`
;; using harpoon 2, it seems trickier as the cursor position is required
(λ harpoon-file-explorer []
  "harpoon files in buffer or selected in an explorer (Netrw/mini.files)"
  (let [hlist (: (require :harpoon) :list)
        hconfig (. (require :harpoon) :config)
        key hconfig.settings.key
        context {:row 1 :col 0}]
    ;; FIXME: refactor/use a harpoon core function to build `value`
    (if (netrw-buf?)
        (do
          (vim.cmd " let netrw#current_word = netrw#Call('NetrwFile', netrw#Call('NetrwGetWord')) ")
          (let [cw (. vim.g "netrw#current_word")]
            (when (= 0 (vim.fn.isdirectory cw))
              (let [path (require :plenary.path)
                    buf-name (path:new cw)
                    value (buf-name:make_relative (key))]
                (hlist:add {: context : value})))))
        (minifiles-buf?)
        (let [minifiles (require :mini.files)
              (ok res) (pcall (. minifiles :get_explorer_state))]
          (when (and ok (not= res nil))
            (local fs-entry ((. minifiles :get_fs_entry)))
            (when (= :file fs-entry.fs_type)
              (let [path (require :plenary.path)
                    buf-name (path:new fs-entry.path)
                    value (buf-name:make_relative (key))]
                (hlist:add {: context : value})))))
        (hlist:add))))

{1 :theprimeagen/harpoon
 :branch :harpoon2
 :dependencies [:nvim-lua/plenary.nvim]
 :keys [{1 :<leader>a
         2 #(harpoon-file-explorer)
         ;;  👍 : error if the cursor in Netrw or mini.files not on a file
         :desc "Harpoon the current file"}
        {1 :<leader>e
         ;; it was <C-e> but <C-a>/<C-e> = cursor navi in terms
         2 #(#($:toggle_quick_menu (: (require :harpoon) :list)) (. (require :harpoon)
                                                                    :ui))
         :desc "Toggle the harpoon's quick menu"}
        ;; h,t,n,s = keys for Dvorak layout
        ;; (you can add more to access #5+ harpooned files)
        (unpack (nav-file-mapping! :h :t :n :s))]
 :lazy true
 ;; setup as method is MANDATORY
 :config #(let [h (require :harpoon)]
            (h:setup {:settings {:save_on_toggle true}}))}
