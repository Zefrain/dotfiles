-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
-- vim.g.mapleader = ","
-- vim.g.maplocalleader = "\\"

-- Setup lazy.nvim
require("lazy").setup({
	spec = {
        -- UltiSnips and snippets
        { 'SirVer/ultisnips' },
        { 'honza/vim-snippets' },

        -- NERDTree with lazy loading
        {
            'scrooloose/nerdtree',
            cmd = 'NERDTreeToggle', -- Load only when NERDTreeToggle is called
        },

        -- FZF and fzf.vim with a post-install hook
        {
            'junegunn/fzf',
            dir = '~/.fzf',
            build = './install --all', -- Equivalent to vim-plug's `do`
        },
        { 'junegunn/fzf.vim' },

        -- Clang-format integration
        { 'rhysd/vim-clang-format' },
        { 'kana/vim-operator-user' },

        -- CoC for LSP
        {
            'neoclide/coc.nvim',
            branch = 'release', -- Use the release branch
        },

        -- Commentary plugin for easy commenting
        { 'tpope/vim-commentary' },

        -- Markdown-related plugins
        { 'godlygeek/tabular' },
        { 'preservim/vim-markdown' },

        -- Golang support
        { 'fatih/vim-go' },

        -- GitGutter for Git diff signs
        { 'airblade/vim-gitgutter' },

        -- Deoplete for Neovim
        {
            'Shougo/deoplete.nvim',
            cond = vim.fn.has('nvim') == 1, -- Only load in Neovim
            build = ':UpdateRemotePlugins', -- Post-install/update command
        },
        {
            'roxma/nvim-yarp',
            cond = vim.fn.has('nvim') ~= 1, -- Only load if not Neovim
        },
        {
            'roxma/vim-hug-neovim-rpc',
            cond = vim.fn.has('nvim') ~= 1, -- Only load if not Neovim
        },
        -- import your plugins
        {
            "dhananjaylatkar/cscope_maps.nvim",
            dependencies = {
                "nvim-telescope/telescope.nvim", -- optional [for picker="telescope"]
                "ibhagwan/fzf-lua", -- optional [for picker="fzf-lua"]
                "echasnovski/mini.pick", -- optional [for picker="mini-pick"]
            },
            opts = {
                -- USE EMPTY FOR DEFAULT OPTIONS
                -- DEFAULTS ARE LISTED BELOW
            },
        },

        -- colorschemes
        { "catppuccin/nvim", name = "catppuccin", priority = 1000 },
        { "folke/tokyonight.nvim", name = "tokyonight", lazy = true, priority = 1000, opts = {}, }

    },

    -- Configure any other settings here. See the documentation for more details.
    -- colorscheme that will be used when installing plugins.
    install = { colorscheme = { "habamax" } },
    -- automatically check for plugin updates
    checker = { enabled = true },

})
vim.cmd.colorscheme "tokyonight"
