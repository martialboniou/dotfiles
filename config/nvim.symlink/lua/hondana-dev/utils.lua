local M = {}

local message = {
    file_not_attached = "this buffer is not attached to a file",
    file_already_executable = "this file is already executable",
    file_cannot_be_executable = "this file cannot be set as executable",
    file_cannot_be_non_executable = "this file cannot be made non executable",
}

M.is_executable = function()
    local file = vim.fn.expand("%:p")
    local perm = vim.fn.getfperm(file)
    if perm == "" then
        error(message.file_not_attached, 141)
    end
    if string.match(perm, 'x', 3) then
        return true
    else
        return false
    end
end

M.make_executable = function()
    if M.is_executable() then
        error(message.file_already_executable)
    end
    local file = vim.fn.expand("%:p")
    vim.cmd("!chmod u+x " .. file)
    if not M.is_executable() then
        error(message.file_cannot_be_executable)
    end
    return "you can execute " .. file .. " now!"
end

M.toggle_executable = function()
    local is_exe = M.is_executable()
    local file = vim.fn.expand("%:p")
    if is_exe then
        vim.cmd("!chmod a-x " .. file)
        if M.is_executable() then
            error(message.file_cannot_be_non_executable)
        end
        return "noone can execute " .. file .. " anymore."
    end
    vim.cmd("!chmod u+x " .. file)
    if not M.is_executable() then
        error(message.file_cannot_be_executable)
    end
    return "you can execute " .. file .. " now!"
end

M.php_cs_fixer_format = function()
    local file = vim.fn.expand("%:p")
    print([[exe tail -2 $file]])
end

return M
