" functions.vim (Function definitions)

" Function to generate a TOC for a Markdown file
function! GenerateMarkdownTOC()
    " Check if the file is a Markdown file
    if &filetype !=# 'markdown'
        echo "TOC generation is only supported for Markdown files."
        return
    endif

    " Clear any existing TOC lines
    let toc = []
    " Add TOC markers
    call add(toc, '<!-- TOC -->')
    " Iterate over all lines in the buffer
    for lnum in range(1, line('$'))
        let line = getline(lnum)
        " Match lines starting with # (Markdown headers)
        if line =~ '^#'
            let level = len(matchstr(line, '^#*'))
            let heading = substitute(line, '^#\+ *', '', '')
            let sanitized_heading = substitute(tolower(heading), '[^a-z0-9一-龥\-]', '', 'g')
            let link = substitute(sanitized_heading, ' ', '-', 'g')
            let toc_line = repeat('  ', level - 1) . '- [' . heading . '](#' . link . ')'
            call add(toc, toc_line)
        endif
    endfor
    call add(toc, '<!-- /TOC -->')
    " Find and replace the old TOC
    let start_line = search('<!-- TOC -->', 'n')
    let end_line = search('<!-- /TOC -->', 'n')
    if start_line > 0 && end_line > 0
        call deletebufline('%', start_line, end_line)
    endif
    " Insert the new TOC at the top
    call append(0, toc)
endfunction

