(import-macros {: tc} :hondana-dev.macros)
(import-macros {: or=} :hibiscus.core)

(local M {})

(tc type "string[]")
(local message
       {:file_not_attached "this buffer is not attached to a file"
        :file_already_executable "this file is already executable"
        :file_cannot_be_executable "this file cannot be set as executable"
        :file_cannot_be_non_executable "this file cannot be made non executable"})

(tc param ?file string return boolean)
(fn is-executable [?file]
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

(tc param ft string return boolean)
(λ M.lisp-ft? [ft]
  "true if the filetype `ft` has the Lisp syntax"
  (or= ft :fennel :lisp :clojure :scheme :racket :shen :janet :hy))

M
