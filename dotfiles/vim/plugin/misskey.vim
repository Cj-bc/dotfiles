" Vim Plugin for Misskey
"
" This plugin require [Cj-bc/misskey-hs](https://github.com/Cj-bc/misskey-hs)
" as dependency. Please install 'misskey-cli' from the repo before use this
"
" Maintainer: Cj-bc a.k.a Cj.BC_SD <cj.bc-sd@outlook.jp>
" Last Change: 2020 Jan 8

if !executable("misskey-cli")
  finish
endif

" Define visibility of notes you post.
let g:misskey#config#visibility = "Home"


" post the note. Internal use
" misskeyPost :: [String] -> IO ()
function s:misskeyPost(text)
  let msg = join(a:text, "\n")
  let _ = system("misskey-cli -q notes/create --visibility ".g:misskey#config#visibility." --text '" . msg . "'")
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


if exists("*tweetvim#say#command")
  function! s:PostBothTwitterAndMisskey(content)
    call tweetvim#say#command(content)
    call s:misskeyCommandSay(content)
  endfunction

  command -nargs=? MisskeyPostAndTweet :call s:PostBothTwitterAndMisskey(<q-args>)
endif

command -nargs=? MisskeyCommandSay :call s:misskeyCommandSay(<q-args>)
command -nargs=0 MisskeySay :call s:misskeySay()

