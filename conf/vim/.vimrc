
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Multiple Plug commands can be written in a single line using | separators
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" for clang-format
Plug 'rhysd/vim-clang-format'
Plug 'kana/vim-operator-user'

" for lsp
" Plug 'prabirshrestha/vim-lsp'
" Plug 'prabirshrestha/asyncomplete.vim'
" Plug 'mattn/vim-lsp-settings'
" Plug 'ycm-core/YouCompleteMe'
" Plug 'm-pilia/vim-ccls'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" comment
Plug 'tpope/vim-commentary'

" markdown
Plug 'godlygeek/tabular'
Plug 'preservim/vim-markdown'

" YCM
" Plug 'tabnine/YouCompleteMe'

" Golang
Plug 'fatih/vim-go'

if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
autocmd FileType go let g:deoplete#enable_at_startup = 1

" Plug 'ilyachur/cmake4vim'

Plug 'airblade/vim-gitgutter'

call plug#end()

" setup leader key
let mapleader = "," " map leader to comma

" setup for vim-lsp
if executable('pylsp')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pylsp']},
        \ 'allowlist': ['python'],
        \ })
endif

" set leo file as rust
autocmd BufRead,BufNewFile *.leo set filetype=rust

" function! s:on_lsp_buffer_enabled() abort
"     setlocal omnifunc=lsp#complete
"     setlocal signcolumn=yes
"     if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
"     nmap <buffer> <leader>gd <plug>(lsp-definition)
"     nmap <buffer> <leader>gs <plug>(lsp-document-symbol-search)
"     nmap <buffer> <leader>gS <plug>(lsp-workspace-symbol-search)
"     nmap <buffer> <leader>gr <plug>(lsp-references)
"     nmap <buffer> <leader>gi <plug>(lsp-implementation)
"     nmap <buffer> <leader>gt <plug>(lsp-type-definition)
"     nmap <buffer> <leader>rn <plug>(lsp-rename)
"     nmap <buffer> <leader>[g <plug>(lsp-previous-diagnostic)
"     nmap <buffer> <leader>]g <plug>(lsp-next-diagnostic)
"     nmap <buffer> <leader>K <plug>(lsp-hover)
"     " nnoremap <buffer> <expr><c-f> lsp#scroll(+4)
"     " nnoremap <buffer> <expr><c-d> lsp#scroll(-4)

"     let g:lsp_format_sync_timeout = 1000
"     autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')

"     " refer to doc to add more commands
" endfunction

" augroup lsp_install
"     au!
"     " call s:on_lsp_buffer_enabled only for languages that has the server registered.
"     autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
" augroup END

" force refresh completion
imap <c-space> <Plug>(asyncomplete_force_refresh)
" For Vim 8 (<c-@> corresponds to <c-space>):
" imap <c-@> <Plug>(asyncomplete_force_refresh)

" Register ccls C++ lanuage server.
" if executable('ccls')
"    au User lsp_setup call lsp#register_server({
"       \ 'name': 'ccls',
"       \ 'cmd': {server_info->['ccls']},
"       \ 'root_uri': {server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), '.ccls'))},
"       \ 'initialization_options': {'cache': {'directory': expand('~/.cache/ccls') }},
"       \ 'allowlist': ['c', 'cpp', 'objc', 'objcpp', 'cc'],
"       \ })
" endif

" " Key bindings for vim-lsp.
" nn <silent> <M-d> :LspDefinition<cr>
" nn <silent> <M-r> :LspReferences<cr>
" nn <f2> :LspRename<cr>
" nn <silent> <M-a> :LspWorkspaceSymbol<cr>
" nn <silent> <M-l> :LspDocumentSymbol<cr>


set fileencodings=utf-8,gbk,big5

" set tabstop=8
" set shiftwidth=8

if has("syntax") && (&t_Co > 2 || has("gui_running"))
	syntax on
	function! ActivateInvisibleCharIndicator()
		syntax match TrailingSpace "[ \t]\+$" display containedin=ALL
		highlight TrailingSpace ctermbg=Red
	endf
	autocmd BufNewFile,BufRead * call ActivateInvisibleCharIndicator()
endif
" Show tabs, trailing whitespace, and continued lines visually
" set list listchars=tab:»·,trail:·,extends:…

" highlight overly long lines same as TODOs.
set textwidth=200
" autocmd BufNewFile,BufRead *.c,*.h exec 'match Todo /\%>' . &textwidth . 'v.\+/'

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

nmap <leader>sw :call CurtineIncSw()<CR>

" cursor line
set cursorline

" NERDTree
nnoremap <leader>n :NERDTreeFocus<CR>
" nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

" fzf-vim
" nnoremap <Leader>rg :Rg <c-r>=expand('<cword>')<cr><CR>
nnoremap <leader>rg :Rg \b<c-r><c-w>\b<CR>

" Map a key combination to search for selected text with :Rg
vnoremap <leader>rg y:Rg \b<C-R>"\b<CR>
"vnoremap <Leader>rg :Rg "\b" . escape(@", '\\/') . "\b"<CR>

" markdown
let g:vim_markdown_folding_disabled = 1

" folding
set foldmethod=syntax
set nofoldenable
set ts=4 sw=4 expandtab

" clipboard
set clipboard^=unnamed,unnamedplus

" highlight search
set hlsearch

" ignore whitespace for vimdiff
set diffopt+=iwhiteall

" clang-format
map <C-K> <Plug>(operator-clang-format)
imap <C-K> <Plug>(operator-clang-format)
" map to <Leader>cf in C++ code
autocmd FileType c,cpp,objc nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <buffer><Leader>cf :ClangFormat<CR>
" if you install vim-operator-user
autocmd FileType c,cpp,objc map <buffer><Leader>x <Plug>(operator-clang-format)
" Toggle auto formatting:
nmap <Leader>C :ClangFormatAutoToggle<CR>
" autocmd FileType c ClangFormatAutoEnable

let s:vimdir = fnamemodify(resolve(expand('<sfile>:p')), ':h')
let s:cscope_path = s:vimdir . '/cscope_maps.vim'
let s:coc_path = s:vimdir . '/coc.vim'

" GitGutter
highlight GitGutterAdd    guifg=#009900 ctermfg=2
highlight GitGutterChange guifg=#bbbb00 ctermfg=3
highlight GitGutterDelete guifg=#ff2222 ctermfg=1

let g:gitgutter_line_highlight = 1

:execute "source " . s:cscope_path
:execute "source " . s:coc_path
