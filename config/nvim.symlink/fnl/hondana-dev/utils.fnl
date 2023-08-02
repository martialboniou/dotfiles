(local M {})

(local message
       {:file_not_attached "this buffer is not attached to a file"
        :file_already_executable "this file is already executable"
        :file_cannot_be_executable "this file cannot be set as executable"
        :file_cannot_be_non_executable "this file cannot be made non executable"})

(λ M.is_executable []
  (let [file (vim.fn.expand "%:p")
        perm (vim.fn.getfperm file)]
    (when (= perm "") (error message.file_not_attached 141))
    (not= nil (string.match perm :x 3))))

(λ M.make_executable []
  (when (M.is_executable) (error message.file_already_executable))
  (let [file (vim.fn.expand "%:p")]
    (vim.cmd (.. "!chmod u+x " file))
    (when (not (M.is_executable)) (error message.file_cannot_be_executable))
    (.. "you can execute " file " now!")))

(fn M.toggle_executable []
  (let [is-exe (M.is_executable)
        file (vim.fn.expand "%:p")]
    (if is-exe
        (do
          (vim.cmd (.. "!chmod a-x " file))
          (when (M.is_executable)
            (error message.file_cannot_be_non_executable))
          (.. "noone can execute " file " anymore."))
        (do
          (vim.cmd (.. "!chmod u+x " file))
          (when (not (M.is_executable))
            (error message.file_cannot_be_executable))
          (.. "you can execute " file " now!")))))

M
