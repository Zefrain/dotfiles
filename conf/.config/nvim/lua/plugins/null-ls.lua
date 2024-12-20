return {
  {
    "jose-elias-alvarez/null-ls.nvim",
    opts = function(_, opts)
      local null_ls = require("null-ls")
      opts.sources = vim.list_extend(opts.sources or {}, {
        null_ls.builtins.diagnostics.shellcheck, -- Show linting issues
        null_ls.builtins.code_actions.shellcheck, -- Apply ShellCheck fixes
      })
    end,
  },
}
