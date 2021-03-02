
setlocal omnifunc=lsp#complete
nmap <buffer> <leader>R <plug>(lsp-rename)
nmap <buffer> <leader>D <plug>(lsp-definition)
nmap <buffer> <leader>r <plug>(lsp-references)
nmap <buffer> <leader>w <plug>(lsp-workspace-symbol)
nmap <buffer> <leader>f <plug>(lsp-type-definition)
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
