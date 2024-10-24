;; inspired by https://github.com/zk-org/zk-nvim/blob/main/lua/zk/root_pattern_util.lua
(local M {})

;; BEWARE: cannot be cross-compiled
(macro is-windows []
  (let [uname (vim.uv.os_uname)]
    (-> :Windows (uname.version:match) (not= nil))))

(set M.path (let [is-fs-root #(if (is-windows)
                                  ($:match "^%a:$")
                                  (= "/" $))
                  dirname (fn [path]
                            (let [strip-dir-pat "/([^/]+)$"
                                  strip-sep-pat "/$"]
                              (when (and path (not= 0 (length path)))
                                (let [result (: (path:gsub strip-sep-pat "")
                                                :gsub strip-dir-pat "")]
                                  (if (= 0 (length result))
                                      (if (is-windows)
                                          (: (path:sub 1 2) :upper)
                                          "/")
                                      result)))))]
              {:escape-wildcards #($:gsub "([%[%]%?*])" "\\%1")
               :exists #(let [stat (vim.uv.fs_stat $)]
                          (-> stat
                              (and stat.type)
                              (or false)))
               :join #(-> [$...]
                          (vim.tbl_flatten)
                          (table.concat "/"))
               :iterate-parents #(let [it (fn [_ v]
                                            (when (and v (not (is-fs-root v)))
                                              (let [v (dirname v)]
                                                (when (and v
                                                           (vim.uv.fs_realpath v))
                                                  (values v $)))))]
                                   (values it $ $))}))

(fn M.search-ancestors [startpath func]
  (vim.validate {:func [func :f]})
  (if (func startpath)
      startpath
      (do
        (var guard 99)
        (each [path (M.path.iterate-parents startpath) &until (= 0 guard)]
          (set guard (- guard 1))
          (when (func path)
            (lua "return path"))))))

(fn M.root-pattern [...]
  (let [patterns (vim.tbl_flatten [...])
        matcher (fn [path]
                  (each [_ pattern (ipairs patterns)]
                    (each [_ p (ipairs (-> path
                                           (M.path.escape-wildcards)
                                           (M.path.join pattern)
                                           (vim.fn.glob true true)))]
                      (when (M.path.exists p) (lua "return path")))))]
    #(M.search-ancestors $ matcher)))

(fn M.find-project-root [startpath root-subdirectory]
  (let [startpath (if (-> startpath
                          (vim.fn.isdirectory)
                          (not= 1))
                      startpath
                      (.. startpath "/."))]
    (each [dir (vim.fs.parents startpath)]
      (when (->> root-subdirectory
                 (vim.fs.joinpath dir)
                 (vim.fn.isdirectory)
                 (= 1))
        (lua "return dir")))))

M
