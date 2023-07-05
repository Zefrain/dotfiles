function! FindFunction()
  " Search backwards for the previous '{' character that is not inside a comment or string
  let l:line = search('\(\%([^/\\\\]\|^\)\(\/\/\|\/\*\|\\\"\|\x27\)[^{]*\)\@<!{', 'bnW')
  if l:line > 0
    " Move to the beginning of the line
    normal! 0
    " If the line starts with a keyword (e.g. "function" or "class"), move to the end of the keyword
    if getline('.') =~ '^\s*\%(function\|class\|struct\)\>'
      normal! /
      norm! e
    endif
  endif
endfunction

nnoremap [[ :call FindFunction()<CR>
nnoremap ]] :call FindFunction()<CR>

