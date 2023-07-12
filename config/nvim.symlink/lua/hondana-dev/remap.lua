vim.g.mapleader = " "
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)
vim.keymap.set("n", "<leader><leader>", vim.cmd.Ex) -- fast nav

-- Q is removed (that's good!)
vim.keymap.set("n", "Q", "<nop>")

-- mode the selection up & down with K & J
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- vim.keymap.set("n", "Y", "yg$")
vim.keymap.set("n", "J", "mzJ`z") -- doesn't move the cursor while appending line
vim.keymap.set("n", "<C-d>", "<C-d>zz") -- page down
vim.keymap.set("n", "<C-u>", "<C-u>zz") -- page up
vim.keymap.set("n", "n", "nzzzv") -- keep the cursor in the middle during search
vim.keymap.set("n", "N", "Nzzzv") -- keep the cursor in the middle during backsearch

-- greatest remap ever
--   paste a buffer but doesn't keep the deleted selection
--   so you can paste the same again
-- vim.keymap.set("x", "<leader>p", "\"_dP")

-- next greatest remap ever : asbjornHaland
-- yank/delete for the clipboard
vim.keymap.set("n", "<leader>y", "\"+y")
vim.keymap.set("v", "<leader>y", "\"+y")
vim.keymap.set("n", "<leader>Y", "\"+Y")
vim.keymap.set("n", "<leader>d", "\"_d")
vim.keymap.set("v", "<leader>d", "\"_d")

-- ThePrimeagen thing; can be changed when foot controller is plugged
vim.keymap.set("i", "<C-c>", "<Esc>")

vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>") -- require https://github.com/ThePrimeagen/.dotfiles/blob/master/bin/.local/scripts/tmux-sessionizer in your path

vim.keymap.set("n", "<leader>f", function()
	vim.lsp.buf.format() -- TODO: should not be here
end)

-- quickfix navigation
vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
-- vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true }) -- TODO: something better

--[[
vim.keymap.set("n", "<leader><leader>", function()
    vim.cmd("so")
end)
]]

