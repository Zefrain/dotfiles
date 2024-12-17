-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- Function to reload all configuration files in 'lua/config'
function ReloadConfig()
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
end

-- Map <leader>rc to reload all config files
vim.keymap.set("n", "<leader>rc", ReloadConfig, { noremap = true, silent = true, desc = "Reload all configs" })
