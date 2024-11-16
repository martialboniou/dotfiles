(import-macros {: tc} :hondana-dev/macros)
;; inspired by https://github.com/zk-org/zk-nvim/blob/main/lua/zk/root_pattern_util.lua
(local M {})

;; NOTE: cannot be cross-compiled
(macro root-comparator-or-root [path only-root]
  "returns a multi-value with the filesystem root according to the operating
  system and a comparator function matching the root itself"
  (if (= :Windows _G.jit.os)
      (if only-root
          `(: (: ,path :sub 1 2) :upper)
          `(fn [,path]
             (if (or (not ,path) (= "" ,path)) (error "wrong filesytem")
                 ;; :else
                 (: ,path :match "^%a:$"))))
      (if only-root
          "/"
          `(fn [,path] (= "/" ,path)))))

(tc class Path)
(tc field escape-wildcards "fun(name: string): string")
(tc field exists "fun(path: string): boolean")
(tc field join "fun(...: string?): string")
(tc field iterate-parents "fun(path: string): string_iterator, string, string")
(local Path (let [dirname (fn [path]
                            (let [strip-dir-pat "/([^/]+)$"
                                  strip-sep-pat "/$"]
                              (when (and path (not= 0 (length path)))
                                (let [result (: (path:gsub strip-sep-pat "")
                                                :gsub strip-dir-pat "")]
                                  (if (= 0 (length result))
                                      (root-comparator-or-root path true)
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
                                            (when (and v
                                                       (not (root-comparator-or-root v)))
                                              (let [v (dirname v)]
                                                (when (and v
                                                           (vim.uv.fs_realpath v))
                                                  (values v $)))))]
                                   (values it $ $))}))

(tc type Path)
(set M.path Path)

(tc type matcher)
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

(tc param ... string? return "fun(string): string")
(fn M.root-pattern [...]
  ;; FIX: tbl_flatten is deprecated
  (let [patterns (vim.tbl_flatten [...])
        matcher (fn [path]
                  (each [_ pattern (ipairs patterns)]
                    (each [_ p (ipairs (-> path
                                           (M.path.escape-wildcards)
                                           (M.path.join pattern)
                                           (vim.fn.glob true true)))]
                      (when (M.path.exists p) (lua "return path")))))]
    #(M.search-ancestors $ matcher)))

(tc param startpath string param root_subdirectory string return :nil|string)
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
