"  I'll use vim-lsp for LSP client, so I disable this.
" let g:ale_cpp_gcc_executable = 'g++'
" let g:ale_cpp_gcc_options = '-std=c++17 -Wall'
let g:ale_cpp_clang_options = '-std=c++17 -Wall'
let g:ale_cpp_ccls_eecutable = ''
let g:ale_linters = {
    \ 'sh': ['language_server'],
    \ 'cpp': ['ccls'],
    \ 'python': ['pyls', 'mypy', 'flake8'],
    \ 'haskell': ['stack-ghc', 'hie'],
    \ }
"    \ 'elm': ['elm-language-server'],
"
let g:ale_linters_ignore = { 'elm': ['make'] } " Avoid conflicts with elm-ls
let g:ale_elm_ls_use_global = 1 " Use globally installed elm-ls
