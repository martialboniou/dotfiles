(import-macros {: tc} :hondana-dev.macros)
(import-macros {: setlocal!} :hibiscus.vim)

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

(local hondana-focus-stamp [:diagnostic
                            :icon
                            :file
                            :icon
                            :file
                            :arrow
                            :norm
                            :buffer
                            :column
                            :percent
                            :norm])

(local hondana-defocus-stamp [:diagnostic
                              :icon
                              :file
                              :icon
                              :file
                              :defocusArrow
                              :norm
                              :buffer
                              :column
                              :percent
                              :norm])

(macro make-stamps! [list]
  `(icollect [_# s# (ipairs ,list)]
     (.. "%#Status" ;;
         ;; capitalize an ascii word
         (-> (s#:sub 1 1) (: :upper) (.. (s#:sub 2))) "#")))

(local {: api : uv} vim)

;; F = utility functions
(local F {:au api.nvim_create_autocmd
          :augrp #(api.nvim_create_augroup $ {:clear true})})

;; highlight status by side-effect (background first, foreground is optional)
(highlight-status! {StatusDiagnostic ["#b16286" "#1d2021"]
                    StatusPercent ["#b16286" "#1d2021"]
                    StatusModified ["#1d2021" "#d3869b"]
                    StatusIcon ["#fabd2f" "#f12222"]
                    StatusIcon2 ["#fabd2f" "#b16286"]
                    ;; StatusIcon2 ["#fabd2f" "#f15386"]
                    ;; StatusIcon2 ["#fabd2f" "#d15344"]
                    StatusFile ["#fabd2f" "#1d2021"]
                    StatusArrow ["#458588" "#fabd2f"]
                    StatusDefocusArrow ["#1d2021" "#fabd2f"]
                    StatusBuffer ["#98971a" "#1d2021"]
                    StatusBranch ["#458588" "#1d2021"]
                    StatusColumn ["#1d2021" "#ebdbb2"]})

(F.au :ColorScheme
      {:group (F.augrp :Hondana_RestoreStatusLine)
       :callback #(do
                    (highlight-status! {StatusLine ["#458588" "#1d2021"]})
                    (highlight-status! {winbar [:NONE]}))})

(tc type "string[]")
(local stamps (make-stamps! hondana-focus-stamp))

(tc type "string[]")
(local defocus-stamps (make-stamps! hondana-defocus-stamp))

(tc return "string")
(fn yield [t] (or (table.remove t 1) ""))

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

(var diagnostics "")

;; HACK: trying to make a diagnostic summary somewhere
(fn _G.show_diagnostics [] diagnostics)

(fn statusline-diagnostics []
  (let [results []]
    (var right-spacing " ")
    (for [severity 1 4]
      (local n (->> {: severity} (vim.diagnostic.get 0) (length)))
      (when (> n 0)
        (var result-format "%s %d")
        (var icon (-> :hondana-dev.utils.globals (require)
                      (. :icons :diagnostic) (?. severity)))
        (set right-spacing " ")
        (when (not icon)
          (set icon (-> vim.diagnostic.severity
                        (. severity)
                        (: :sub 1 1)
                        (.. ":")))
          (set result-format "%s%d ")
          (set right-spacing ""))
        (->> n (string.format result-format icon) (table.insert results))))
    (set diagnostics
         (-> results (table.concat) (#(if (= "" $) $ (.. " " $ right-spacing)))))))

(fn _G.show_column []
  (let [col vim.fn.col
        max (col :$)
        curr (col :.)]
    (if (< curr max)
        ;; col('$') - 1 is the last colunm in normal mode
        (.. curr "/" (- max 1))
        ;; empty line or inserting at the end of the current line
        (.. " " max))))

;; INFO: uncomment this if you want to display modes instead of filetype when not in normal mode/unknown mode
;; (fn _G.get_mode_or_filetype []
;;   "display the mode or the filetype when normal/unknown"
;;   (local padding {:prefix "  " :postfix " "})
;;   (local format #(.. padding.prefix $ padding.postfix))
;;   (local modes (-> :hondana-dev.utils.globals (require) (. :modes)))
;;   (or (-?> (?. modes (vim.fn.mode) :text) (format))
;;       (if (= "" vim.bo.filetype) ""
;;           (format (.. (vim.bo.filetype:upper) " ")))))
;; (local filetype-info "%{%v:lua.get_mode_or_filetype()%}")

;; I prefer a simpler filetype-info (comment this when you uncomment the previous code)
(local filetype-info "%{&ft==''?'':'  '..toupper(&ft)..'  '}")

(local diagnostic-info "%{%v:lua.show_diagnostics()%}")
(local readonly-tag "%{&readonly?'   ':' '}")
(local modified-tag "%{&modified?' ':''}")
;; NOTE: 燐 : %c can be enough here; no need %l AKA line (b/c always known when sync'd)
(local column-info " %{%v:lua.show_column()%} ")

(tc type string)
;; TODO: when focused, replace the filetype by the mode except for normal
(local hondana-statusline
       (let [stamp #(yield stamps)]
         (.. filetype-info (stamp) diagnostic-info (stamp) readonly-tag (stamp)
             "%F" (stamp) modified-tag (stamp) (stamp) "" "" (stamp) "%="
             (stamp) " ⌂ %n " (stamp) column-info (stamp) "  %p%% " (stamp)
             "%{get(b:,'gitbranch','')}")))

(tc type string)
(local hondana-defocus-statusline ;; change stamp to the defocus list
       (let [stamp #(yield defocus-stamps)]
         (.. filetype-info (stamp) diagnostic-info (stamp) readonly-tag (stamp)
             "%F" (stamp) modified-tag (stamp) (stamp) "" (stamp) "%="
             (stamp) " ⌂ %n " (stamp) column-info (stamp) "  %p%% " (stamp)
             "%{get(b:,'gitbranch','')}")))

;; set `b:gitbranch` as used in statusline each time we enter a buffer, window...
(F.au [:BufWinEnter :BufNew]
      {:group (F.augrp :Hondana_StatusLine_GetGitBranch)
       :callback statusline-git-branch})

;; refresh diagnostics in statusline
(F.au [:DiagnosticChanged :BufWinEnter]
      {:group (F.augrp :Hondana_StatusLine_Diagnostics)
       :callback statusline-diagnostics})

;; active/unactive statusline on focusing events
(let [group (F.augrp :Hondana_StatusLine)]
  (F.au [:BufEnter :WinEnter]
        {: group :callback #(setlocal! statusline hondana-statusline)})
  (F.au [:BufLeave :WinLeave]
        {: group :callback #(setlocal! statusline hondana-defocus-statusline)}))
