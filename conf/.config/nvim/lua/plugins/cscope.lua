return {
  "Zefrain/cscope_maps.nvim",
  dependencies = {
    "nvim-telescope/telescope.nvim", -- optional [for picker="telescope"]
    -- "ibhagwan/fzf-lua", -- optional [for picker="fzf-lua"]
    -- "echasnovski/mini.pick", -- optional [for picker="mini-pick"]
  },

  config = function()
    require("cscope_maps").setup({
      -- maps related defaults
      disable_maps = false, -- "true" disables default keymaps
      skip_input_prompt = false, -- "true" doesn't ask for input
      prefix = "<C-\\>", -- prefix to trigger maps

      -- cscope related defaults
      cscope = {
        -- location of cscope db file
        db_file = "./cscope.out", -- DB or table of DBs
        -- NOTE:
        --   when table of DBs is provided -
        --   first DB is "primary" and others are "secondary"
        --   primary DB is used for build and project_rooter
        -- cscope executable
        exec = "cscope", -- "cscope" or "gtags-cscope"
        -- choose your fav picker
        picker = "telescope", -- "quickfix", "telescope", "fzf-lua" or "mini-pick"
        -- size of quickfix window
        qf_window_size = 5, -- any positive integer
        -- position of quickfix window
        qf_window_pos = "bottom", -- "bottom", "right", "left" or "top"
        -- "true" does not open picker for single result, just JUMP
        skip_picker_for_single_result = true, -- "false" or "true"
        -- these args are directly passed to "cscope -f <db_file> <args>"
        db_build_cmd_args = { "-bqkvC" },
        -- statusline indicator, default is cscope executable
        -- statusline_indicator = nil,
        -- try to locate db_file in parent dir(s)
        project_rooter = {
          enable = false, -- "true" or "false"
          -- change cwd to where db_file is located
          change_cwd = false, -- "true" or "false"
        },
      },

      -- stack view defaults
      stack_view = {
        tree_hl = true, -- toggle tree highlighting
      },
    })

    -- -- Example custom keymaps for cscope commands
    -- local map = vim.api.nvim_set_keymap
    -- local opts = { noremap = true, silent = true }
    --
    -- map("n", "<leader>cs", ":Cscope find s <C-r><C-w><CR>", opts) -- Find symbol
    -- map("n", "<leader>cg", ":Cscope find g <C-r><C-w><CR>", opts) -- Find global definition
    -- map("n", "<leader>cc", ":Cscope find c <C-r><C-w><CR>", opts) -- Find calls
  end,
}
