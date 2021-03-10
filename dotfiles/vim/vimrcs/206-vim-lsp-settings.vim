let g:lsp_settings = {
    \ 'haskell-language-server': {
    \   'cmd': ['haskell-language-server-wrapper-macOS', '--lsp'],
    \   'workspace_config': {'haskell-language-server-wrapper-macOS': {'plugins': {'formattingProvider': 'stylish-haskell' }}}
    \   },
\ }
