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
(fn get-buffers [?patterns]
  (local buffers [])
  (local {: fnamemodify : bufname : getbufvar : bufnr} vim.fn)
  (local patterns (if ?patterns ?patterns
                      {:modified "&modified"
                       :readonly "&readonly"
                       :modifiable "&modifiable"}))
  (var bad-pattern false)
  (for [nr 1 (bufnr "$")]
    (when (F.is-primary-buffer nr)
      ;; WARN: MUST break when v is not an elligible pattern (eg. "&modified"/"&readonly"/"&modifiable")
      (local flags (collect [k v (pairs patterns)
                             &until (and (-> nr (getbufvar v) (type)
                                             (= :string))
                                         (do
                                           (set bad-pattern true)
                                           (vim.notify (.. "bufferline: `get-buffers()` has received an unknown pattern `"
                                                           v
                                                           "` as second argument of `vim.fn.getbufvar()`")
                                                       vim.log.levels.ERROR)
                                           true))]
                     (values k (-> nr (getbufvar v) (= 1)))))
      ;; ensure the imperfect flags and the rest of the buffers are not recorded
      (when bad-pattern (lua :break))
      (table.insert buffers
                    {: flags
                     :bufnr nr
                     :current (-> "%" (bufnr) (= nr))
                     :name (-> nr (bufname) (fnamemodify ":t"))})))
  ;; return an empty record on error
  ;; otherwise process normally by checking the unicity of the `:name` field
  (if bad-pattern [] (do
                       (F.unique-tail buffers)
                       buffers)))

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
