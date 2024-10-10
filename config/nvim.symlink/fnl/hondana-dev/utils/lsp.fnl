;; generic LSP interface
;; source/idea: https://github.com/zk-org/zk-nvim/blob/main/lua/zk/lsp.lua

(local Lsp {})
(set Lsp.__index Lsp)

(fn Lsp.new [self name ?config]
  (local config (or ?config {}))
  (setmetatable {;; our instance
                 : name
                 : config
                 :_client-id nil} self))

(fn Lsp.external-client [self]
  (let [ac-symbol (if (= 1 (vim.fn.has :nvim-0.10)) ;; 
                      :get_clients :get_active_clients)
        active-clients ((. vim.lsp ac-symbol) {:name self.name})]
    (when (not= nil (next active-clients))
      ;; return first lsp server that is actually in use
      (each [_ v (ipairs active-clients)]
        (when (not= nil (next v.attached_buffers))
          (lua "return v.id"))))))

(fn Lsp.start [self ?config]
  (when ?config (set self.config ?config))
  (when (not self._client-id)
    (set self._client-id (self:external-client)))
  (when (not self._client-id)
    (set self._client-id (vim.lsp.start_client self.config))))

(fn Lsp.buf-add [self ?bufnr]
  (self:start)
  (vim.lsp.buf_attach_client (or ?bufnr 0) self._client_id))

(fn Lsp.stop [self]
  (let [client (self:client)]
    (when client (client.stop))
    (set self._client-id nil)))

(fn Lsp.client [self]
  (vim.lsp.get_client_by_id self._client-id))

(fn Lsp.get-name [self]
  self.name)

Lsp
