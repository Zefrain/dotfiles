" set runtimepath^=~/.vim runtimepath+=~/.vim/after
" let &packpath = &runtimepath
" source ~/.vimrc

" bootstrap lazy.nvim, LazyVim and your plugins
lua require("config.lazy")
