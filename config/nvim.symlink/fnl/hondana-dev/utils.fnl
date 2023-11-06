(local message
       {:file_not_attached "this buffer is not attached to a file"
        :file_already_executable "this file is already executable"
        :file_cannot_be_executable "this file cannot be set as executable"
        :file_cannot_be_non_executable "this file cannot be made non executable"})

(λ is_executable [?file]
  (let [file-in-buffer? (= nil ?file)
        file (or ?file (vim.fn.expand "%:p"))
        perm (vim.fn.getfperm file)]
    (when (and file-in-buffer? (= perm ""))
      (error message.file_not_attached 141))
    ;; TODO: directory? check
    (-> perm (string.reverse) (string.match :x 7) (not= nil))))

(λ make_executable []
  (when (is_executable) (error message.file_already_executable))
  (let [file (vim.fn.expand "%:p")]
    (vim.cmd (.. "!chmod u+x " file))
    (when (not (is_executable)) (error message.file_cannot_be_executable))
    (.. "you can execute " file " now!")))

(λ toggle_executable []
  (let [is-exe (is_executable)
        file (vim.fn.expand "%:p")]
    (if is-exe
        (do
          (vim.cmd (.. "!chmod a-x " file))
          (when (is_executable)
            (error message.file_cannot_be_non_executable))
          (.. "noone can execute " file " anymore."))
        (do
          (vim.cmd (.. "!chmod u+x " file))
          (when (not (is_executable))
            (error message.file_cannot_be_executable))
          (.. "you can execute " file " now!")))))

{: is_executable : toggle_executable}
