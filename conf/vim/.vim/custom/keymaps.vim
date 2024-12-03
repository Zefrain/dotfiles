" keymaps.vim (Key mappings)

" Setup leader key
let mapleader = ","

" NERDTree key bindings
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-f> :NERDTreeFind<CR>

" fzf-vim key bindings
nnoremap <leader>rg :Rg \b<c-r><c-w>\b<CR>
vnoremap <leader>rg y:Rg \b<C-R>"\b<CR>

" Cursor line
set cursorline

" Comment toggle
nmap <leader>sw :call CurtineIncSw()<CR>

" clang-format key mappings
map <C-K> <Plug>(operator-clang-format)
imap <C-K> <Plug>(operator-clang-format)
autocmd FileType c,cpp,objc nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <buffer><Leader>cf :ClangFormat<CR>
nmap <Leader>C :ClangFormatAutoToggle<CR>

" EasyAlign key mappings
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" Function to create a TOC for Markdown files
autocmd FileType markdown nnoremap <buffer> <leader>toc :call GenerateMarkdownTOC()<CR>

