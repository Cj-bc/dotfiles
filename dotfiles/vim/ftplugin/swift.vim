setlocal comments=s1:/*,mb:*,ex:*/,:///,://
setlocal expandtab
setlocal ts=2
setlocal sw=2
setlocal smartindent

function FormatSwift()
  let l:lines="all"
  pyf expand('~/.vim/swift-format.py')
endfunction

map <C-I> :FormatSwift<cr>
imap <C-I> <c-o>:FormatSwift<cr>
