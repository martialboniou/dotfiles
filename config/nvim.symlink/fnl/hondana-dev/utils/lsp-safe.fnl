(import-macros {: tc} :hondana-dev.macros)
;; generic safe LSP interface
;; source/idea: https://github.com/zk-org/zk-nvim/blob/main/lua/zk/lsp.lua
;; check lsp-types.fnl for common types

;; this table is a placeholder for your persistent init data (better left empty)
(local SafeLsp {})
(set SafeLsp.__index SafeLsp)

;; USAGE
;; example in a lua file made by `zk init && zk new --title="Test Lsp"
;
;; (let [SafeLsp (require :hondana-dev.utils.lsp-safe)]
;;   (do
;;     (-> {:name :zk-foo :cmd [:zk :lsp]}
;;         (#(doto (SafeLsp:new $)
;;             (: :start)
;;             (#(-> ($:client) (. :name) (= :zk-foo) (#(when $ (print :checked)))))
;;             (: :stop)))))
;;   nil)

(tc param config SafeLspClientConfig ;;
    param ?options table "table from a subclass" ;;
    return SafeLsp)

(fn SafeLsp.new [self config ?options]
  (when (not config.name)
    (error "must pass a name field"))
  (let [o (vim.tbl_extend :force {: config} (or ?options {}))]
    ;; private slot
    (tc type integer|nil)
    (var client-id nil)

    (fn self.start [self]
      (when (not client-id)
        (set client-id (self:external-client)))
      (when (not client-id)
        (set client-id (vim.lsp.start_client self.config))))

    (fn self.buf-add [self ?bufnr]
      (self:start)
      (tc cast client_id integer)
      (vim.lsp.buf_attach_client (or ?bufnr 0) client-id))

    (fn self.stop [self]
      (let [client (self:client)]
        (when client (client.stop))
        (set client-id nil)))

    (fn self.client [_]
      (tc cast client_id integer)
      (vim.lsp.get_client_by_id client-id))

    (setmetatable o self)))

(tc return :nil|number)
(fn SafeLsp.external-client [self]
  (let [ac-symbol (if (= 1 (vim.fn.has :nvim-0.10)) ;; 
                      :get_clients :get_active_clients)
        active-clients ((. vim.lsp ac-symbol) {:name self.config.name})]
    (when (not= nil (next active-clients))
      ;; return first lsp server that is actually in use
      (each [_ v (ipairs active-clients)]
        (when (not= nil (next v.attached_buffers))
          (lua "return v.id"))))))

(tc class "SafeLspClientConfig: vim.lsp.ClientConfig" ;;
    field name string additional slot for name)

(tc class SafeLsp ;;
    field config SafeLspClientConfig ;;
    ;; metamethods accessing private slots
    field start "fun(self: SafeLsp): nil" ;;
    field stop "fun(self: SafeLsp): nil" ;;
    field buf-add "fun(self: SafeLsp, ?bufnr: number): boolean" ;;
    field client "fun(_: any): vim.lsp.Client?" "client rpc object")

SafeLsp
