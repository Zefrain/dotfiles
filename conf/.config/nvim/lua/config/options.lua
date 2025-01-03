-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
--

-- Override yanky.nvim settings
vim.g.yanky = {
  ring = {
    history_length = 10000, -- Set desired history length (default is 100)
  },
}

local opt = vim.opt
opt.relativenumber = false

vim.g.autoformat = false
