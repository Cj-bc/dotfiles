" Vim Plugin for Misskey
"
" This plugin require [Cj-bc/misskey-hs](https://github.com/Cj-bc/misskey-hs)
" as dependency. Please install 'misskey-cli' from the repo before use this
"
" Maintainer: Cj-bc a.k.a Cj.BC_SD <cj.bc-sd@outlook.jp>
" Last Change: 2020 Jan 8


" post the note. Internal use
" misskeyPost :: [String] -> IO ()
function s:misskeyPost(text)
  let msg = join(a:text, "\n")
  let _ = system("misskey-cli -q notes/create --text '" . msg . "'")
  redraw | echo ''
endfunction

" almost same as tweetVimCommandSay
"
" misskeyCommandSay :: String -> IO ()
function s:misskeyCommandSay(text)
  let msg = empty(a:text) ? input('note : ') : a:text
  if empty(msg)
    redraw | echo '' | return
  endif
  redraw
  echo msg
  call s:misskeyPost([msg])
endfunction

" almost same as tweetVimSay
"
" misskeySay :: IO ()
function s:misskeySay()
    call s:misskeyvimSayOpen()
endfunction

" Open new buffer to write note content. Internal use
"
"misskeySayOpen :: IO Buffer
function! s:misskeyvimSayOpen()
  " Create window
  botright split 'misskeyvim_say'
  2 wincmd _
  setlocal modifiable

  let &filetype = 'misskeyvim_say'
  nnoremap <buffer> <silent> <CR> :call <SID>misskeyPost(getbufline('misskeyvim_say', 1, "$")) \| q!<CR>

  startinsert!

  setlocal nomodified
endfunction



if executable("misskey-cli")
  command -nargs=? MisskeyCommandSay :call s:misskeyCommandSay(<q-args>)
  command -nargs=0 MisskeySay :call s:misskeySay()
endif


