(import-macros {: tc} :hondana-dev.macros)

;; inspired by https://github.com/ojroques/nvim-hardline/lua/hardline/bufferline.lua
;; used by `hondana-dev.status`

(local fmt string.format)
(local F {})

;; unused
(fn _to-section [buffer index ?show-index]
  (local flags [])
  (local name (. buffer :name))
  (var item (if (= "" name) "[No Name]" name))
  (when (. buffer :flags :modified)
    (table.insert flags "[+]"))
  (when (not (. buffer :flags :modifiable))
    (table.insert flags "[-]"))
  (when (. buffer :flags :readonly)
    (table.insert flags "[RO]"))
  (let [info (table.concat flags)]
    (set item (if (= "" info) item (fmt "%s %s" item info))))
  (set item (if ?show-index (fmt " %d:%s " index item) (fmt " %s " item)))
  (let [{: current :flags {: modified}} buffer]
    {: item : modified : current}))

(tc return "Buffer[]")
(fn get-buffers [?modifiable-only]
  (local buffers [])
  (var flags {:modified "&modified"})
  (when (not ?modifiable-only)
    (set flags.modifiable "&modifiable")
    (set flags.readonly "&readonly"))
  (for [nr 1 (vim.fn.bufnr "$")]
    (when (F.is-primary-buffer nr)
      (table.insert buffers
                    {:bufnr nr
                     :name (-> nr (vim.fn.bufname) (vim.fn.fnamemodify ":t"))
                     :current (-> "%" (vim.fn.bufnr) (= nr))
                     :flags (collect [k v (pairs flags)]
                              (values k (-> nr (vim.fn.getbufvar v) (= 1))))})))
  (F.unique-tail buffers)
  buffers)

(tc param bufnr integer "number of buffer")
(tc return boolean)
(fn F.is-primary-buffer [bufnr]
  "true if `bufnr` is the number of a file buffer"
  (let [{: buflisted : getbufvar} vim.fn]
    (not (or (= 0 (buflisted bufnr)) (= :qf (getbufvar bufnr "&filetype"))
             (= :terminal (getbufvar bufnr "&buftype"))))))

(fn F.unique-tail [buffers]
  (local hist [])
  (local size (length buffers))
  (var dup false)
  (for [i 1 size]
    (local buffer (. buffers i))
    (local name (. buffer :name))
    (local hname (. hist name))
    (if (not hname)
        (set (. hist name) 1)
        (when (not= "" name)
          ;; first time a hist[name] is greater than 1, dup = true
          (when (not dup)
            (set dup true)
            (local parent
                   (-> buffer (#[(. $ :bufnr) name]) (unpack) (vim.fn.bufname)
                       (F.get-head)))
            (set (. buffer :name) (fmt "%s/%s" parent name)))
          (set (. hist name) (+ 1 hname)))))
  (when (not dup) (lua :return))
  (F.unique-tail buffers))

(fn F.get-head [path tail]
  (local modify vim.fn.fnamemodify)
  (var result (modify path ":p"))
  (local size (-> tail (vim.split :/) (length)))
  (for [_ 1 size]
    (set result (modify result ":h")))
  (modify result ":t"))

;; types
(tc class Buffer)
(tc field bufnr integer)
(tc field name string)
(tc field current boolean)
(tc field flags "{ modified: boolean, modifiable: boolean, readonly: boolean }")

;; export
{: get-buffers}
