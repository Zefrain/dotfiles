return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
  },
  {
    "Mofiqul/dracula.nvim",
  },
  {
    "projekt0n/github-nvim-theme",
    name = "github-theme",
  },
  { "ellisonleao/gruvbox.nvim" },
  {
    "navarasu/onedark.nvim",
    name = "onedark",
  },
  {
    "rebelot/kanagawa.nvim",
    name = "kanagawa",
  },
  {
    "rose-pine/neovim",
    name = "rose-pine",
  },
  {
    "shaunsingh/nord.nvim",
    -- lazy = false, -- 确保主题立即加载（非延迟加载）
    priority = 1000, -- 高优先级，确保在其他插件前加载
    config = function()
      vim.g.nord_contrast = true -- 启用对比度增强
    end,
  },
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      style = "night",
    },
  },
}
