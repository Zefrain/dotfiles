local group = vim.api.nvim_create_augroup("CscopeBuild", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = { "*.c", "*.h,", "*.cpp" },
  callback = function()
    vim.cmd("Cscope db build")
  end,
  group = group,
})
