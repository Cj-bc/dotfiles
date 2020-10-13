xmap <silent>sa <Plug>(operator-surround-append)
nmap <silent>sa <Plug>(operator-surround-append)
xmap <silent>sd <Plug>(operator-surround-delete)
nmap <silent>sd <Plug>(operator-surround-delete)
xmap <silent>sr <Plug>(operator-surround-replace)
nmap <silent>sr <Plug>(operator-surround-replace)
let g:operator#surround#blocks = {
  \ '-' : [
  \   {'block': ['{{{', '}}}'], 'motionwise': ['line', 'block'], 'keys': ['f']},
  \   ],
  \ 'haskell' : [
  \   {'block': ['-- {{{', '}}}'], 'motionwise': ['line', 'block'], 'keys': ['f']},
  \   ],
  \ 'markdown' : [
  \   {'block': ['```', '```'], 'motionwise': ['line'], 'keys': ['```']},
  \   ],
  \ 'markdown.vimwiki.qfix_memo' : [
  \   {'block': ['```', '```'], 'motionwise': ['line'], 'keys': ['```']},
  \   ],
  \ }


