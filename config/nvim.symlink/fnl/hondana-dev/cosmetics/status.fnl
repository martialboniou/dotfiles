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

(macro unzip-stamps! [keys list]
  "`keys` MUST be literal at comptime"
  (let [tbl {}]
    (each [i k (ipairs keys)]
      (local seq
             `(icollect [_# s# (ipairs ,list)]
                (let [t# (if (-> s# (type) (= :string)) [s#] s#)
                      v# ,(if (> i 1)
                              ;; get the n-th element (or the first one when nil)
                              `(if (. t# ,i) (. t# ,i) (. t# 1))
                              ;; get the first element
                              `(. t# 1))]
                  (.. "%#Status" ;;
                      ;; capitalize an ascii word
                      (-> (v#:sub 1 1) (: :upper) (.. (v#:sub 2) "#"))))))
      (set (. tbl k) seq))
    tbl))

(local {: api : uv} vim)

;; F = utility functions
(local F {:au api.nvim_create_autocmd
          :augrp #(api.nvim_create_augroup $ {:clear true})})

(tc alias StatusElement "string|{[1]: string, [2]: string}")

(tc type "StatusElement[]")
(local hondana-stamps [:diagnostic
                       :icon
                       :file
                       :icon
                       :file
                       [:arrow :defocusArrow]
                       :norm
                       :buffer
                       :column
                       :percent
                       :norm])

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
                    StatusColumn ["#1d2021" "#ebdbb2"]})

(F.au :ColorScheme
      {:group (F.augrp :Hondana_RestoreStatusLine)
       :callback #(do
                    (highlight-status! {StatusLine ["#458588" "#1d2021"]})
                    (highlight-status! {winbar [:NONE]}))})

(tc return "string")
(fn yield [t] (or (table.remove t 1) ""))

(tc type string)
(local no-git-default-tag "")

(fn refresh-gitbranch [bufnr data]
  ;; VimL MUST NOT be used within a loop's callback: call `vim.schedule` to delay this
  (vim.schedule #(vim.fn.setbufvar bufnr "gitbranch" data)))

;; TODO: improve with cache
(fn statusline-git-branch []
  "Async'ly replace the buffer variable `b:gitbranch`"
  ;; ensure the path is related to the file, not the working directory
  ;; (IMO no need to manage mini.files' or even term's cases)
  (tc diagnostic disable)
  (local (ok _) (pcall vim.cmd "lcd %:p:h"))
  (when (not ok) (lua :return))
  (local [cmd & args] [:git :-C (uv.cwd) :rev-parse :--abbrev-ref :HEAD])
  ;; keep the `bufnr` (`vim.b.gitbranch` is a variable of the current buffer,
  ;; not a specific variable of the buffer where this async fn has been
  ;; proc'd)
  ;; NOTE: don't use `vim.b.gitbranch` in an async fn
  (local bufnr (vim.fn.bufnr :%))
  (tc diagnostic enable)
  ;; restore the current working directory after `vim.uv.cwd()`
  (vim.cmd "lcd -")
  ;; spawn a process
  (var handle nil)
  (tc cast handle uv.uv_handle_t)
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
            (when (not= 0 $)
              (refresh-gitbranch bufnr no-git-default-tag))))
  (set handle (spawn cmd options on-exit))
  (for [i 2 3]
    (let [p (. stdio i)]
      (when p
        (start p
               (fn [_ data]
                 (when data
                   ;; format the detected branch
                   (refresh-gitbranch bufnr
                                      (.. "  " (data:gsub "\n" "") " ")))))))))

(local {: posix :icons {:diagnostic diagnostic-icons}}
       (require :hondana-dev.utils.globals))

(fn unsaved-other-buffers []
  "returns the number "
  (let [{: get-buffers} (require :hondana-dev.utils.bufferline)]
    (accumulate [count 0 _ buffer (ipairs (get-buffers {:modified "&modified"}))]
      (let [{: current :flags {: modified}} buffer]
        ;; (if (and (not current) modified)
        ;;     (+ 1 count)
        ;;     count)
        (if modified
            (+ 1 count)
            count)))))

(fn statusline-diagnostics []
  (let [results []]
    (var right-spacing " ")
    (for [severity 1 4]
      (local n (->> {: severity} (vim.diagnostic.get 0) (length)))
      (when (> n 0)
        (var result-format "%s %d")
        (var icon (. diagnostic-icons severity))
        (set right-spacing " ")
        (when (not icon)
          (set icon (-> vim.diagnostic.severity
                        (. severity)
                        (: :sub 1 1)
                        (.. ":")))
          (set result-format "%s%d ")
          (set right-spacing ""))
        (->> n (string.format result-format icon) (table.insert results))))
    (set vim.b.diagnostics
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

;; (set vim.o.fillchars "stl:o")

(fn _G.build_filename [spacing]
  (local ({:fn {: expand : strchars : pathshorten} :fs {: joinpath}} {:nvim_win_get_width width})
         (values vim api))
  (local filename (expand "%:p:~"))
  (local win-size (width 0))
  ;; `strchars` knows UNICODE, not Lua's `#`
  (local text-fit? #(-> $ (strchars) (+ spacing) (< win-size)))
  (if (text-fit? filename)
      filename
      (let [subdirs (case (values posix (expand "%:p:~:h"))
                      (true :/)
                      :/
                      ;; Windows case (ie posix = false): untested
                      (where (false text)
                             (let [last #(do
                                           (var dump nil)
                                           (each [next $...] (set dump next))
                                           dump)]
                               ;; last = utility fn to get the last iterator
                               (-> filename (vim.fs.parents) (last) (= text))))
                      text
                      ;; otherwise append the correct separator (most common except editing at root level)
                      (_ text)
                      (joinpath (pathshorten text) ""))
            short-filename (.. subdirs (expand "%:t"))]
        ;; shrink to the max but the parent directory if enough space
        (if (text-fit? short-filename) short-filename (pathshorten filename)))))

(local (info tag)
       (values {:diagnostic "%{get(b:,'diagnostics','')}"
                ;; NOTE: 燐 : %c can be enough here; I don't need %l AKA line
                :column " %{%v:lua.show_column()%} "
                :buffer-number "  %n "
                :git-branch "%{get(b:,'gitbranch','')}"
                ;; 50 = average number of additional characters for the filename to shrink
                ;; to a shorten version according to the window width
                :filename "%{%v:lua.build_filename(50)%}"}
               {:readonly "%{&readonly?'   ':' '}"
                :modified "%{&modified?'  ':''}"}))

(local stamps (unzip-stamps! [:focus :defocus] hondana-stamps))

;; append the stamp fn to F
;; (set F.stamp #(yield stamps))

(tc type string)
(local hondana-statusline
       (let [stamp #(yield stamps.focus)]
         (.. filetype-info (stamp) info.diagnostic (stamp) tag.readonly (stamp)
             info.filename (stamp) tag.modified (stamp) (stamp) "" ""
             (stamp) "%=" (stamp) info.buffer-number (stamp) info.column (stamp)
             "  %p%% " (stamp))))

(tc type string)
(local hondana-defocus-statusline ;; change stamp to the defocus list
       (let [stamp #(yield stamps.defocus)]
         (.. filetype-info (stamp) info.diagnostic (stamp) tag.readonly (stamp)
             info.filename (stamp) tag.modified (stamp) (stamp) "" (stamp)
             "%=" (stamp) info.buffer-number (stamp) info.column (stamp)
             "  %p%% " (stamp) info.git-branch)))

;; set `b:gitbranch` as used in statusline each time we enter a buffer, window...
(F.au [:BufWinEnter :BufNew]
      {:group (F.augrp :Hondana_StatusLine_GetGitBranch)
       :callback statusline-git-branch})

;; refresh diagnostics in statusline
(F.au [:DiagnosticChanged :BufEnter :WinEnter]
      {:group (F.augrp :Hondana_StatusLine_Diagnostics)
       :callback statusline-diagnostics})

;; active/unactive statusline on focusing events
(let [group (F.augrp :Hondana_StatusLine)]
  (F.au [:BufEnter :WinEnter]
        {: group :callback #(setlocal! statusline hondana-statusline)})
  (F.au [:BufLeave :WinLeave]
        {: group :callback #(setlocal! statusline hondana-defocus-statusline)}))
