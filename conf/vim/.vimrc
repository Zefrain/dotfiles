
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'

" Multiple Plug commands can be written in a single line using | separators
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" for clang-format
Plug 'kana/vim-operator-user'

" for switch header and source file
Plug 'ericcurtin/CurtineIncSw'

" Initialize plugin system
call plug#end()


set fileencodings=utf-8,gbk,big5

set tabstop=8
set shiftwidth=8

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
set textwidth=80
autocmd BufNewFile,BufRead *.c,*.h exec 'match Todo /\%>' . &textwidth . 'v.\+/'

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)


" cursor line 
set cursorline
