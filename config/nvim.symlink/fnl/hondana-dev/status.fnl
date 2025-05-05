(import-macros {: tc} :hondana-dev.macros)
(import-macros {: set!} :hibiscus.vim)

(macro highlight-status! [tbl]
  "generate the highlights with a dict of tag as keys and a sequence of 1 or 2 Vim color codes
  as values as value. The first color code is background, the second one as foreground is optional"
  (let [out []]
    (each [type colors (pairs tbl)]
      (assert-compile (sequence? colors) "expected a sequence for colors"
                      colors)
      (local args (collect [k v (pairs {:bg (. colors 1) :fg (. colors 2)})]
                    (when v
                      (values k (tostring v)))))
      ;; the order is irrelevant
      (table.insert out `(api.nvim_set_hl 0 ,(tostring type) ,args)))
    `(do
       ,(unpack out))))

(macro make-stamps! []
  (icollect [_ s (ipairs [:type
                          :file
                          :modified
                          :norm
                          :buffer
                          :percent
                          :diagnostic
                          :location])]
    (.. "%#Status" ;;
        ;; capitalize an ascii word
        (-> (s:sub 1 1) (: :upper) (.. (s:sub 2))) "#")))

(local {: api : uv} vim)

;; F = utility functions
(local F {:au api.nvim_create_autocmd
          :augrp #(api.nvim_create_augroup $ {:clear true})})

(tc type boolean)
(var flip true)

;; highlight status by side-effect (background first, foreground is optional)
(highlight-status! {StatusType ["#b16286" "#1d2021"]
                    StatusFile ["#fabd2f" "#1d2021"]
                    StatusModified ["#1d2021" "#d3869b"]
                    StatusBuffer ["#98971a" "#1d2021"]
                    StatusLocation ["#458588" "#1d2021"]
                    StatusPercent ["#1d2021" "#ebdbb2"]
                    StatusDiagnostic ["#b16286" "#1d2021"]})

(F.au :ColorScheme
      {:group (F.augrp :Hondana_RestoreStatusLine)
       :callback #(highlight-status! {StatusLine ["#458588" "#1d2021"]
                                      winbar [NONE]})})

;; INFO: simplify the statusline => no more chevron
(tc return string)
(local bump #"")
;; (fn bump []
;;   "alternate the chevron's orientation"
;;   (set flip (not flip))
;;   (if flip
;;       "‹"
;;       "›"))

(tc type "string[]")
(local stamps (make-stamps!))

(tc return "string")
(fn stamp []
  "yields next status"
  (or (table.remove stamps 1) ""))

(tc type string)
(local no-git-default-tag "")

;; TODO: improve with cache
(fn statusline-git-branch []
  "Async'ly replace the buffer variable `b:gitbranch`"
  ;; ensure the path is related to the file, not the working directory
  ;; (IMO no need to manage mini.files' or even term's cases)
  (tc diagnostic disable)
  (local (ok _) (pcall vim.cmd "lcd %:p:h"))
  (when (not ok) (lua :return))
  (local [cmd & args] [:git :-C (uv.cwd) :rev-parse :--abbrev-ref :HEAD])
  (tc diagnostic enable)
  ;; restore the current working directory after `vim.uv.cwd()`
  (vim.cmd "lcd -")
  ;; spawn a process
  (var handle nil)
  (tc cast handle uv_handle_t)
  (local {:new_pipe new : spawn :read_start start :read_stop stop : close} uv)
  (local stdio [nil (new) (new)])
  (local options {: args : stdio})
  (local on-exit
         #(do
            (for [i 2 3]
              (let [p (. stdio i)]
                (when p (stop p) (close p))))
            (close handle)
            ;; default tag on error
            (when (not= 0 $) (set vim.b.gitbranch no-git-default-tag))))
  (set handle (spawn cmd options on-exit))
  (for [i 2 3]
    (let [p (. stdio i)]
      (when p
        (start p
               (fn [_ data]
                 (when data
                   ;; format the detected branch"
                   (set vim.b.gitbranch (.. "  " (data:gsub "\n" "") " ")))))))))

;; TODO: get the items
(local icons ;; ""
       {:diagnostics {:error "✘" :warning "▲" :hint "⚑" :info "»"}
        :buffers {:readonly "󰌾" :modified "●" :unsaved_others "○"}})

(local diagnostics-attrs
       [[vim.diagnostic.severity.ERROR icons.diagnostics.error]
        [vim.diagnostic.severity.WARN icons.diagnostics.warning]
        [vim.diagnostic.severity.HINT icons.diagnostics.hint]
        [vim.diagnostic.severity.INFO icons.diagnostics.info]])

(var diagnostics "")

;; HACK: trying to make a diagnostic summary somewhere
(fn _G.show_diagnostics [] diagnostics)

(fn statusline-diagnostics []
  (let [results []]
    (for [i 1 4]
      (local [severity icon] (. diagnostics-attrs i))
      (local n (->> {: severity} (vim.diagnostic.get 0) (length)))
      (when (> n 0)
        (table.insert results (string.format "%s %d" icon n))))
    (set diagnostics (table.concat results " "))))

(tc type string)
(set! statusline (-> [(.. "%{%v:lua.show_diagnostics()%}" (stamp) (bump))
                      "%Y  "
                      (.. (bump) (stamp) (bump))
                      (.. "%F" ;; "%{get(b:,'gitbranch','')}"
                          )
                      (.. (bump) (stamp))
                      "%h%m%r"
                      (.. (stamp) "%=" (stamp) (bump))
                      ;; "﬘ %n"
                      "⌂ %n"
                      (.. (bump) (stamp) (bump))
                      ;; "燐"
                      ;; no need %l AKA line (always known)
                      " %c"
                      (.. (bump) (stamp) (bump))
                      ;; "%p%% "
                      ;; "%p%%"
                      " %p%%"
                      (.. (bump) (stamp) "%{get(b:,'gitbranch','')}")]
                     (table.concat " ")))

;; set `b:gitbranch` as used in statusline each time we enter a buffer, window...
(F.au [:BufWinEnter :BufNew]
      {:group (F.augrp :Hondana_StatusLine_GetGitBranch)
       :callback statusline-git-branch})

;; refresh diagnostics in statusline
(F.au [:DiagnosticChanged :BufWinEnter]
      {:group (F.augrp :Hondana_StatusLine_Diagnostics)
       :callback statusline-diagnostics})
