-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

vim.api.nvim_create_autocmd("VimLeave", {
    callback = function()
        os.execute("clear")
    end,
})

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = { "*.c", "*.h,", "*.cpp" },
  callback = function()
    vim.cmd("Cscope db build")
  end,
  group = group,
})
