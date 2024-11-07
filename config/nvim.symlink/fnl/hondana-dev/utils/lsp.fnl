(import-macros {: tc} :hondana-dev.macros)
;; generic LSP interface
;; source/idea: https://github.com/zk-org/zk-nvim/blob/main/lua/zk/lsp.lua

;; this table is a placeholder for your persistent init data (better left empty)
(tc class Lsp ;;
    field config vim.lsp.ClientConfig ;;
    field private _client-id? number?)

(local Lsp {})
(set Lsp.__index Lsp)

;; USAGE
;; example in a lua file made by `zk init && zk new --title="Test Lsp"
;
;; (let [Lsp (require :hondana-dev.utils.lsp)]
;;   (do
;;     (-> {:name :zk-foo :cmd [:zk :lsp]}
;;         (#(doto (Lsp:new $)
;;             (: :start)
;;             (#(-> ($:client) (. :name) (= :zk-foo) (#(when $ (print :checked)))))
;;             (: :stop)))))
;;   nil)

;; from vim.lsp
(tc class vim.lsp.ClientConfig field name string additional slot for name)

(tc class vim.lsp.Client field stop "fun(force?: boolean)")

(tc param config vim.lsp.ClientConfig param ?options table
    "table from a subclass")

(tc return Lsp)
(fn Lsp.new [self config ?options]
  (when (not config.name)
    (error "must pass a name field"))
  (let [o (vim.tbl_extend :force {: config} (or ?options {}))]
    (setmetatable o self)
    o))

(tc return :nil|number)
(fn Lsp.external-client [self]
  (let [ac-symbol (if (= 1 (vim.fn.has :nvim-0.10)) ;; 
                      :get_clients :get_active_clients)
        active-clients ((. vim.lsp ac-symbol) {:name self.config.name})]
    (when (not= nil (next active-clients))
      ;; return first lsp server that is actually in use
      (each [_ v (ipairs active-clients)]
        (when (not= nil (next v.attached_buffers))
          (lua "return v.id"))))))

(fn Lsp.start [self]
  (when (not self._client-id)
    (set self._client-id (self:external-client)))
  (when (not self._client-id)
    (set self._client-id (vim.lsp.start_client self.config))))

(tc param ?bufnr number return boolean)
(fn Lsp.buf-add [self ?bufnr]
  (self:start)
  (vim.lsp.buf_attach_client (or ?bufnr 0) self._client_id))

(fn Lsp.stop [self]
  (let [client (self:client)]
    (when client (client.stop))
    (set self._client-id nil)))

(tc return vim.lsp.Client? client rpc object)
(fn Lsp.client [self]
  (vim.lsp.get_client_by_id self._client-id))

Lsp
