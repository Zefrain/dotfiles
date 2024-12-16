" vimrc (Main configuration file)

" Initialize vim-plug
call plug#begin('~/.vim/plugged')

" Plugin declarations
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'junegunn/fzf.vim'
Plug 'rhysd/vim-clang-format'
Plug 'kana/vim-operator-user'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-commentary'
Plug 'godlygeek/tabular'
Plug 'preservim/vim-markdown'
Plug 'fatih/vim-go'
Plug 'airblade/vim-gitgutter'

" Deoplete for Neovim
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

call plug#end()

" Set leader key
let mapleader = ","

" Source all .vim files in the custom directory
let custom_dir = expand('~/.vim/custom')
if isdirectory(custom_dir)
    for file in split(globpath(custom_dir, '*.vim'), '\n')
        execute 'source' file
    endfor
endif

" Markdown
let g:vim_markdown_folding_disabled = 1

" Folding settings
set foldmethod=syntax
set nofoldenable
set ts=4 sw=4 expandtab

" Clipboard settings
set clipboard^=unnamed,unnamedplus

" Highlight search
set hlsearch

" GitGutter settings
highlight GitGutterAdd    guifg=#009900 ctermfg=2
highlight GitGutterChange guifg=#bbbb00 ctermfg=3
highlight GitGutterDelete guifg=#ff2222 ctermfg=1

autocmd FileType c,cpp syntax enable
