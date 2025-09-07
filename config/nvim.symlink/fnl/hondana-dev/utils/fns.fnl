;; some utility functions
;; no goal for now but may merge the content of `root-pattern` here later
(import-macros {: tc} :hondana-dev.macros)

(local M {})

;; utility: spawn-pipe
(tc param command "string[]"
    "command as array; first is the binary; rest = args")

(tc param on_result "fun(data: string)")
(tc param on_error "fun(error_code?: number)")
(fn M.spawn-pipe [command on-result on-error]
  "Spawn a command with `libuv` & process according to the returned data."
  (var handle nil)
  (tc cast handle uv.uv_handle_t)
  (local uv vim.uv)
  (local {:new_pipe new : close} uv)
  (local stdio [nil (new) (new)])
  (tc diagnostic disable)
  (local [cmd & args] command)
  (tc diagnostic enable)
  (local options {: args : stdio})
  (local on-exit #(do
                    (for [i 2 3]
                      (let [p (. stdio i)]
                        (when p (uv.read_stop p) (close p))))
                    (close handle)
                    (when (and on-error (not= 0 $))
                      (vim.schedule #(on-error $)))))
  (set handle (uv.spawn cmd options on-exit))

  (fn on-read [_ data]
    (when data
      (vim.schedule #(on-result data))))

  (for [i 2 3]
    (let [p (. stdio i)]
      (when p (uv.read_start p on-read)))))

;; utility: tbl-reverse
(tc generic "U: any")
(tc generic "V: any")
(tc param tbl "table<integer, U>" param ?fun "fun(U): V")
(tc return "table<integer, V>")
(fn M.tbl-reverse [tbl ?fun]
  (let [rev []
        fun (or ?fun #$)
        tally (length tbl)]
    (for [i tally 1 -1]
      (->> i (. tbl) (fun) (table.insert rev)))
    rev))

;; utility: fibonacci
(tc class BigNumber)
;; non-exhaustive list of fields
(tc field maxExp integer)
(tc field minExp integer)
(tc field negative boolean)
(tc field nan boolean)
(tc field infinity boolean)
(tc field base integer)
(tc field isZero "fun(): boolean")
(tc field toBase "fun(n: BigNumber, base: integer): table")
(tc param num "integer|BigNumber")
(tc return BigNumber)
(fn M.fibonacci [num]
  "fibonacci function. requires lua-bignumber & lua-ext libraries.
  returns a bignumber (uses `__tostring` to extract data)"
  (local (ok big) (pcall require :bignumber))
  (when (not ok)
    (error "fibonacci: install lua-bignumber"))
  (var n num)
  (if (-> num (type) (= :number))
      (do
        (when (> 0 num)
          (error "expect a strictly positive number"))
        ;; convert to bignumber
        (set n (big num)))
      ;; :else
      (do
        (local (ok zero) (pcall big.isZero n))
        (when (not ok)
          (error "expect a bignumber"))
        (when (or zero (= true n.negative))
          (error "expect a strict positive number"))))
  (var (v1 v2 v3) (values (big 1) (big 1) (big 0)))
  (tc cast n "-integer")
  (local n (n:toBase 2))
  (for [i (- n.maxExp 1) 0 -1]
    (let [rec (. n i)
          calc (* v2 v2)]
      (set (v1 v2 v3) (values (-> v1 (* v1) (+ calc)) (-> v1 (+ v3) (* v2))
                              (-> v3 (* v3) (+ calc))))
      (when (= 1 rec)
        (set (v1 v2 v3) (values (+ v1 v2) v1 v2)))))
  ;; :return
  v2)

;; return a root_dir function for lsp
;; (check `fnl/after/lsp`)
(fn M.root-dir [...]
  (let [files [...]]
    (fn [bufnr on-dir]
      (let [fname (vim.api.nvim_buf_get_name bufnr)]
        (-?> :hondana-dev.utils.root-pattern
             (require)
             (. :root-pattern)
             (#(($ (unpack files)) fname))
             (on-dir))))))

M
