(local message
       {:file_not_attached "this buffer is not attached to a file"
        :file_already_executable "this file is already executable"
        :file_cannot_be_executable "this file cannot be set as executable"
        :file_cannot_be_non_executable "this file cannot be made non executable"})

(λ is-executable [?file]
  "returns a boolean; true if the current buffer file or this file is executable"
  (let [file-in-buffer? (= nil ?file)
        file (or ?file (vim.fn.expand "%:p"))
        perm (vim.fn.getfperm file)]
    (when (and file-in-buffer? (= perm ""))
      (error message.file_not_attached 141))
    ;; TODO: directory? check
    (-> perm (string.reverse) (string.match :x 7) (not= nil))))

(λ _make-executable []
  "make the current buffer file executable for the owner if not already"
  (when (is-executable) (error message.file_already_executable))
  (let [file (vim.fn.expand "%:p")]
    (vim.cmd (.. "!chmod u+x " file))
    (when (not (is-executable)) (error message.file_cannot_be_executable))
    (.. "you can execute " file " now!")))

(λ toggle-executable []
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

{: is-executable : toggle-executable}
