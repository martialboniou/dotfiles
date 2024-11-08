;; fennel-ls: macro-file
(local type-annotations [:class
                         :field
                         :type
                         :alias
                         :as
                         :param
                         :return
                         :async
                         :deprecated
                         :diagnostic
                         :enum
                         :generic
                         :meta
                         :module
                         :operator
                         :package
                         :source
                         :version
                         :overload
                         :cast])

{:funcall! (λ [m f & args]
             "call a function from its module; these modules are cached; useful for callbacks in keybindings"
             `((. (require ,m) ,f) ,(when args (unpack args))))
 :**! (λ [str times]
        "repeat a literal string"
        (assert-compile (< 0 times))
        (assert-compile (= :string (type str)))
        (let [out []]
          (for [_ 1 times] (table.insert out str))
          `(.. ,(unpack out))))
 :tc (λ [...]
       "make a Lua escape hatch for typechecking with lua-language-server.
       tc stands for typechecking"
       (var starter# true)
       ;; the second token will NEVER be an annotation (otherwise, use another tc macro)
       (var post-anno# false)
       `(lua ,(let [filter-optional #($:gsub "%?([%w|_|-]+)" "_3f%1?")
                    chunk# (-> (icollect [_# token# (ipairs [...])]
                                 (let [str# (tostring token#)]
                                   (if starter#
                                       (do
                                         (set starter# false)
                                         (set post-anno# true)
                                         (.. "---@" str#))
                                       (if post-anno#
                                           (do
                                             (set post-anno# false)
                                             (.. " " str#))
                                           (do
                                             (var annotation false)
                                             (each [_# a# (ipairs type-annotations)]
                                               (when (str#:match (.. "^" a# "$"))
                                                 (set annotation true)
                                                 (lua :break)))
                                             (if annotation
                                                 (do
                                                   (set post-anno# true)
                                                   (.. "\n---@" str#))
                                                 ;; pipe case
                                                 (if (str#:match "^|.*")
                                                     (.. "\n---" str#)
                                                     (.. " " str#))))))))
                               (table.concat))
                    ;; TODO: not efficient!
                    chunk# (filter-optional chunk#)]
                chunk#)))}
