;; TODO: slimv (compare with emacs.sly)
(import-macros {: g!} :hibiscus.vim)
{1 :monkoose/nvlime
 :ft :lisp
 :config #(
          do
          (vim.cmd "call nvlime#plugin#Setup()")   
          ; (g! :nvlime_options
          ;    {:leader ","
          ;     :implementation :sbcl
          ;     :address {:host :127.0.0.1 :port 7002}})
          )
 :dependencies :monkoose/parsley}
