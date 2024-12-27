-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- Function to reload all configuration files in 'lua/config'

-- Map <leader>rc to reload all config files
vim.keymap.set("n", "<leader>rc", function()
  vim.cmd("source $MYVIMRC")

  -- Get the path to your config directory
  local config_path = vim.fn.stdpath("config") .. "/lua/config"

  -- Iterate through all Lua files in 'config' directory
  for _, file in ipairs(vim.fn.glob(config_path .. "/*.lua", true, true)) do
    local module_name = file:match("lua/(.*)%.lua$"):gsub("/", ".")
    if module_name then
      package.loaded[module_name] = nil -- Unload the module
      require(module_name) -- Reload the module
    end
  end
  print("All config files reloaded!")
end, { noremap = true, silent = true, desc = "Reload all configs" })

vim.keymap.set("n", "<leader>si", function()
  local command = vim.fn.input("Bash command: ")
  if command ~= "" then
    local output = vim.fn.system(command)
    -- Strip leading/trailing whitespace
    output = output:gsub("^%s+", ""):gsub("%s+$", "")
    vim.api.nvim_put({ output }, "l", true, true)
  end
end, { noremap = true, silent = true, desc = "Insert inline Bash result without whitespace" })

-- Keymap to apply quickfix-only code actions
vim.keymap.set("n", "<leader>cF", function()
  vim.lsp.buf.code_action({
    context = {
      only = { "quickfix" }, -- Only apply quickfixes
    },
  })
end, { desc = "Fix Diagnostics" })
