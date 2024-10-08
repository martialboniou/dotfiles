(local M {})

(local message
       {:file_not_attached "this buffer is not attached to a file"
        :file_already_executable "this file is already executable"
        :file_cannot_be_executable "this file cannot be set as executable"
        :file_cannot_be_non_executable "this file cannot be made non executable"})

(local zk-brackets-prn-fn #(values "%[%[(.-)%]%]" "%1"))

(λ is-executable [?file]
  "returns a boolean; true if the current buffer file or this file is executable"
  (let [file-in-buffer? (= nil ?file)
        file (or ?file (vim.fn.expand "%:p"))
        perm (vim.fn.getfperm file)]
    (when (and file-in-buffer? (= perm ""))
      (error message.file_not_attached 141))
    ;; TODO: directory? check
    (-> perm (string.reverse) (string.match :x 7) (not= nil))))

(λ M.toggle-executable []
  "change the current buffer file's executability for the owner"
  (let [is-exe (is-executable)
        file (vim.fn.expand "%:p")]
    (if is-exe
        (do
          (vim.cmd (.. "!chmod a-x " file))
          (when (is-executable)
            (error message.file_cannot_be_non_executable))
          (.. "noone can execute " file " anymore."))
        (do
          (vim.cmd (.. "!chmod u+x " file))
          (when (not (is-executable))
            (error message.file_cannot_be_executable))
          (.. "you can execute " file " now!")))))

(λ M.create-and-open-zk-note []
  "idea: https://github.com/mischavandenburg/dotfiles/blob/main/nvim/lua/config/zettelkasten.lua"
  (let [line (vim.api.nvim_get_current_line)
        (brackets-prn capture-prn) (zk-brackets-prn-fn)
        title (line:match brackets-prn)]
    (if (not title)
        (print "No title found between double square brackets")
        (let [output (-> [:zk :new :--title "\"%s\"" :-p]
                         (table.concat " ")
                         (string.format title)
                         (vim.fn.system))
              file-path (output:match "(.+)\n")
              debug-print #(-> $ (table.concat " ") (print))]
          (when file-path
            (doto (file-path:gsub "%z" "")
              (: :gsub "\n" "")
              (: :gsub (.. brackets-prn "$") capture-prn)
              (->> (vim.fn.fnameescape) (.. :badd) (vim.cmd)))
            (-> file-path (vim.fn.bufnr) (vim.api.nvim_set_current_buf))
            (debug-print ["Created and opened new note:" title])
            (debug-print ["File path:" file-path]))
          (when (not file-path)
            (debug-print ["Failed to create note:" title])
            (debug-print ["Command output:" output]))))))

(λ M.yank-and-search-zk-link [] ; paredit-skip: [
  "idea: https://github.com/mischavandenburg/dotfiles/blob/main/nvim/lua/config/zettelkasten.lua"
  (error "don't use")
  (vim.cmd "normal! yi]")
  (let [yanked-text (vim.fn.getreg "\"")
        (brackets-prn capture-prn) (zk-brackets-prn-fn)]
    (yanked-text:gsub brackets-prn capture-prn)
    (if (= "" yanked-text) (print "No text found inside brackets")
        (let [tb (require :telescope.builtin)
              search-dirs (-?> :.zk
                               (vim.fn.isdirectory)
                               (#(if (= 1 $)
                                     nil
                                     (-> "~/Documents/Notebook"
                                         (vim.fn.expand)
                                         (#[$])))))]
          (-> yanked-text (vim.fn.escape "\\.")
              (#(let [prn {:pick_tags $
                           :hidden true
                           :no_ignore true
                           :follow true}]
                  (when search-dirs
                    (tset prn :search_dirs search-dirs))
                  (let [fennel (require :fennel)]
                    (print (fennel.view prn)))
                  prn)) (tb.find_files))))))

M
