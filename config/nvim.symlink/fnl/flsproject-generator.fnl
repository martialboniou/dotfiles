;; this Fennel code doesn't need to be compiled
(when (-> _G.vim (not))
  (error "you cannot execute this without nvim"))

(local (ok fennel) (pcall require :tangerine.fennel.latest))

(when (not ok)

  (error "ensure nvim `vim.opt.rtp` has the tangerine lazy path set before loading this script"))

(local file :flsproject.fnl)

(λ build-flsproject []
  (let [{: view} fennel
        data-lazy-path #(-> :data
                            (_G.vim.fn.stdpath)
                            (.. :/lazy/ $ :/fnl))
        is-directory #(-> $ (_G.vim.fn.isdirectory) (= 1))
        make-path (fn [name ?type]
                    (.. ";" name :/?.fnl
                        (if (and ?type (= :macro ?type))
                            (.. ";" name :/?/init-macros.fnl)
                            "") ";" name :/?/init.fnl))
        append-path (fn [mode ...]
                      (-> (icollect [_ package (ipairs [...])]
                            (if (is-directory package)
                                (make-path package mode)
                                ""))
                          (table.concat)))
        ;; TODO: check if the user path is required if `.` is already present in the (macro-)path
        user-path (-> :config
                      (_G.vim.fn.stdpath)
                      (.. :/fnl))
        tangerine-fennel-path (data-lazy-path :tangerine.nvim)
        hibiscus-macro-path (data-lazy-path :hibiscus.nvim) ;; default paths
        fennel-path "./?.fnl;./?/init.fnl;src/?.fnl;src/?/init.fnl"
        macro-path "./?.fnl;./?/init-macros.fnl;./?/init.fnl;src/?.fnl;src/?/init-macros.fnl;src/?/init.fnl"
        ;; augmented paths
        fennel-path (let [addons (append-path "" tangerine-fennel-path
                                              user-path)]
                      (.. fennel-path addons))
        macro-path (let [addons (append-path :macro hibiscus-macro-path
                                             user-path)]
                     (.. macro-path addons))
        flsproject {: fennel-path
                    : macro-path
                    :lua-version :lua51
                    :extra-globals "vim love unpack"
                    ;; :lints {:globals true}
                    ;; :diagnostics [:E202]
                    }]
    (doto (io.open file :w)
      (: :write (view flsproject))
      (: :close))
    (print "A new flsproject.fnl has been created.\n")))

{:create-command #(_G.vim.api.nvim_create_user_command :FlsProject
                                                       build-flsproject {})}
