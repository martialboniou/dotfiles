(macro nav-file-mapping! [...]
  (local out [])
  (each [unit key (ipairs [...])]
    (table.insert out
                  `{1 ,(.. :<C- key ">")
                    2 #(#($.nav_file ,unit) (require :harpoon.ui))
                    :desc ,(.. "Go to the #" unit " harpooned file")}))
  out)

(local unpack (or table.unpack _G.unpack)) ; for tests only with Lua 5.4

(λ netrw-buf? []
  (let [buf-type (vim.api.nvim_buf_get_option 0 :filetype)]
    (= :netrw buf-type)))

(λ minifiles-buf? []
  (let [buf-type (vim.api.nvim_buf_get_option 0 :filetype)
        buf-name (vim.api.nvim_buf_get_name 0)]
    (or (and (= :minifiles buf-type) (= buf-name "")) (= nil buf-name))))

(λ harpoon-file-explorer []
  "harpoon files in buffer or selected in an explorer (Netrw/mini.files)"
  (let [mark (require :harpoon.mark)]
    (if (netrw-buf?)
        (do
          (vim.cmd " let netrw#current_word = netrw#Call('NetrwFile', netrw#Call('NetrwGetWord')) ")
          (let [path (. vim.g "netrw#current_word")]
            (when (= 0 (vim.fn.isdirectory path))
              (mark.add_file path))))
        (minifiles-buf?)
        (let [minifiles (require :mini.files)
              (ok res) (pcall (. minifiles :get_target_window))]
          (when (and ok (not= res nil))
            (local fs-entry ((. minifiles :get_fs_entry)))
            (when (= :file fs-entry.fs_type)
              (mark.add_file fs-entry.path))))
        (mark.add_file))))

{1 :theprimeagen/harpoon
 :dependencies [:nvim-lua/plenary.nvim]
 :keys [{1 :<leader>a
         2 #(harpoon-file-explorer)
         ;;  👍 : error if the cursor in Netrw or mini.files not on a file
         :desc "Harpoon the current file"}
        {1 :<leader>e
         ;; it was <C-e> but <C-a>/<C-e> = cursor navi in terms
         2 #(#($.toggle_quick_menu) (require :harpoon.ui))
         :desc "Toggle the harpoon's quick menu"}
        ;; h,t,n,s = keys for Dvorak layout
        ;; (you can add more to access #5+ harpooned files)
        (unpack (nav-file-mapping! :h :t :n :s))]
 :lazy true
 :opts {:menu {:width (-> 0 (vim.api.nvim_win_get_width) (- 4))}}}
