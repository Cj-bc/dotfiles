let g:previm_open_cmd = 'open -a Chrome'
augroup PrevimSettings
  autocmd!
  autocmd BufNewFile,BufRead *.{.md,mdwn,mkd,mkdn,mark*} set filetype=markdown
augroup END
