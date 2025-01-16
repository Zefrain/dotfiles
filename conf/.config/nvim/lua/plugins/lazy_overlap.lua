return {
  -- Modified friendly-snippets
  { "rafamadriz/friendly-snippets", enabled = false },
  { "Zefrain/friendly-snippets", enabled = true },

  -- Modified grug-far.nvim
  { "MagicDuck/grug-far.nvim", enabled = false },
  {
    "Zefrain/grug-far.nvim",
    enabled = true,
    keys = {
      {
        "<leader>sr",
        function()
          local grug = require("grug-far")
          -- local ext = vim.bo.buftype == "" and vim.fn.expand("%:e")
          grug.open({
            transient = true,
            prefills = {
              -- filesFilter = ext and ext ~= "" and "*." .. ext or nil,
              search = "\\b" .. vim.fn.expand("<cword>") .. "\\b",
              filesFilter = "*.*",
              focus = "replace",
            },
          })
        end,
        mode = { "n", "v" },
        desc = "Search and Replace",
      },
    },
    opts = { startCursorRow = 3 },
  },
}
