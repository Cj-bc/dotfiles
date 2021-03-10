" This script is written by skanehira, a.k.a gorilla0513
"
" Copyed at 2020-08-17 23:56
" Original source: https://gist.github.com/skanehira/7dd6ed0dc8da8c6e87a11ab70ea83b53
"
" Require:
"   - [skanehira/code2img](https://github.com/skanehira/code2img)
"   - [mattn/twty](https://github.com/mattn/twty)
function! s:echo_err(msg) abort
  echohl ErrorMsg
  echom '[code2img]' a:msg
  echohl None
endfunction

function! s:on_err_vim(ch, msg) abort
  call s:echo_err(a:msg)
endfunction

function! s:exit_twty_cb(tmp_img, tmp_tweet, ch, msg) abort
  call delete(a:tmp_img)
  call delete(a:tmp_tweet)
  echom 'tweet success'
endfunction

function! s:exit_code2imge_cb(tmp_img, tweet, tmp_code, ch, msg) abort
  call delete(a:tmp_code)

  let tmp_tweet = tempname()
  call writefile([a:tweet], tmp_tweet)

  call job_start(['twty', '-m', a:tmp_img, '-ff', '-'], {
        \ 'in_io': 'file',
        \ 'in_name': tmp_tweet,
        \ 'err_cb': function('s:on_err_vim'),
        \ 'exit_cb': function('s:exit_twty_cb', [a:tmp_img, tmp_tweet])
        \ })
endfunction

function! TweetWithImg(first, last, ...) abort
  let lines = getline(a:first, a:last)
  let tmp_code = printf("%s.%s", tempname(), &ft)
  call writefile(lines, tmp_code)

  let tmp_img = tempname()

  let cmd = ['code2img', '-l', '-t', 'solarized-dark', '-ext', &ft, '-o', tmp_img]

  call job_start(cmd, {
        \ 'in_io': 'file',
        \ 'in_name': tmp_code,
        \ 'err_cb': function('s:on_err_vim'),
        \ 'exit_cb': function('s:exit_code2imge_cb', [tmp_img, a:1, tmp_code])
        \ })
endfunction

command! -nargs=? -range TweetWithImg call TweetWithImg(<line1>, <line2>, <q-args>)
