function! s:CjBC_WordCount(first, last, ...)
  let s:lines = getline(a:first, a:last)
  let s:rawWordCount = system("wc -w", s:lines)
  let s:count = substitute(s:rawWordCount, '\D', '', 'g')
  call popup_atcursor(s:count, #{})
endfunction

com! -range WordCount call s:CjBC_WordCount(<line1>, <line2>)

" provide one of vmap,smap,xmap
