return {
    "echasnovski/mini.files", -- better than telescope for my workflow
    opts = {
        windows = {
            preview = true,
            width_focus = 30,
            width_preview = 30,
        },
        options = {
            use_as_default_explorer = true,
        },
        mappings = {
            reveal_cwd = '@', -- as a reminder
        },
    },
    keys = {
        -- <leader><leader> : open/close at the current file location if possible
        {
            "<leader><leader>",
            function()
                local mini_files = require("mini.files")
                if mini_files.get_target_window() ~= nil then
                    return mini_files.close()
                end
                local ok, _ = pcall(mini_files.open, vim.api.nvim_buf_get_name(0), true)
                if not ok then -- say, the file is not created yet
                    print("Mini.files cannot open the current directory; open cwd instead")
                    -- open at the root of the project instead
                    mini_files.open(vim.loop.cwd(), true)
                end
            end,
            desc = "Open mini.files (directory of the current file)",
        },
        -- <leader>pv : open at the root of the project
        {
            "<leader>pv",
            function()
                require("mini.files").open(vim.loop.cwd(), true)
            end,
            desc = "Open mini.files (cwd)",
        },
    },
    config = function(_, opts)
        require("mini.files").setup(opts)

        -- add gh to show/hide hidden files
        local show_dotfiles = true

        local filter_show = function(_)
            return true
        end
        local filter_hide = function(fs_entry)
            return not vim.startswith(fs_entry.name, ".")
        end
        local toggle_dotfiles = function()
            show_dotfiles = not show_dotfiles
            local new_filter = show_dotfiles and filter_show or filter_hide
            require("mini.files").refresh({
                content = { filter = new_filter }
            })
        end

        vim.api.nvim_create_autocmd("User", {
            pattern = "MiniFilesBufferCreate",
            callback = function(args)
                local buf_id = args.data.buf_id
                vim.keymap.set("n", "gh", toggle_dotfiles, { buffer = buf_id })
            end,
        })

        -- add relative numbers (on nightly build)
        vim.api.nvim_create_autocmd("User", {
            pattern = "MiniFilesWindowUpdate",
            callback = function(args)
                vim.wo[args.data.win_id].relativenumber = true
            end,
        })
    end,
    --[[
    init = function()
        vim.api.nvim.create_autocmd(
            { "VimEnter" },
            { command = "if bufname(\"%\") == \"\" | echo 1 | endif" }
        )
    end,
    ]]
}
