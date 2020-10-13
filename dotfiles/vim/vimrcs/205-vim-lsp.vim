augroup MyLsp
  "  for debug {{{2
  "    let g:lsp_log_verbose = 0
  "    "  let g:lsp_log_file = "vim-lsp.log"
  "    }}}
  let g:lsp_diagnostics_enabled = 0 " use ALE for diagnostics

  if executable('ccls')
     au User lsp_setup call lsp#register_server({
        \ 'name': 'ccls',
        \ 'cmd': {server_info->['ccls']},
        \ 'root_uri': {server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), '.ccls'))},
        \ 'initialization_options': { 'cacheDirectory': '/tmp/ccls_cache' },
        \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp', 'cc'],
        \ })
      au FileType c,cpp,objc,objcpp,cc call s:configure_lsp()
  endif
  if executable('pyls')
      au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
      au FileType python call s:configure_lsp()
  endif

  if executable('hie-wrapper')
      au User lsp_setup call lsp#register_server({
        \ 'name': 'hie-wrapper',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'hie-wrapper --debug --logfile "${HOME}/hie.log" --lsp']},
        \ 'whitelist': ['haskell'],
        \ })
      au FileType haskell call s:configure_lsp()
  endif

  if executable('elm-language-server')
    au User lsp_setup call lsp#register_server(
      \ { 'name': 'elm-language-server'
      \ , 'cmd': {server_info->[&shell, &shellcmdflag, 'elm-language-server --stdio']}
      \ , 'initialization_options':
      \     { 'runtime': 'node'
      \     , 'elmPath': 'elm'
      \     , 'elmFormatPath': 'elm-format'
      \     , 'elmTestPath': 'elm-test'
      \     , 'rootPetterns': 'elm.json'
      \     }
      \ , 'whitelist': ['elm']
      \ })
    au FileType elm call s:configure_lsp()
  endif

  if executable('css-languageserver')
    au User lsp_setup call lsp#register_server(
      \ { 'name': 'css-languageserver'
      \ , 'cmd': {server_info->[&shell, &shellcmdflag, 'css-languageserver --stdio']}
      \ , 'whitelist': ['css','less','sass']
      \ })
    au FileType css,less,sass call s:configure_lsp()
  endif


  func! s:configure_lsp()
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
  endfunc
augroup end
