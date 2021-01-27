nnoremap <leader>gd :Gvdiff<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gc :Gcommit<CR>

command -nargs=1 Gnewbranch Git switch -c <f-args>
