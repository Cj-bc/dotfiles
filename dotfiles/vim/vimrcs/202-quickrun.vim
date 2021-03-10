let g:quickrun_config = deepcopy(g:quickrun#default_config)
let g:quickrun_config['haskell'] = {'command': 'stack',
                                   \'cmdopt': 'runhaskell',
                                   \}
let g:quickrun_config['haskell.hspec'] = {'exec': '%c %o'
                                        \,'command': 'stack'
                                        \,'cmdopt': 'test'
                                        \,'runner': 'terminal'
                                        \}
let g:quickrun_config['python.interactive'] = deepcopy(g:quickrun_config['python'])
let g:quickrun_config['python.interactive']['exec'] = 'python %s'
let g:quickrun_config['python.interactive']['runner'] = 'terminal'

" TODO: make module for this
" I want to add 'nnoremap q <C-w>q' at the end of terminal,
" but I couldn't though 'hook' seems to be the key.
let g:quickrun_config['markdown'] = {'exec': 'mdr %s'
                                    \, 'runner': 'terminal'
                                    \, 'runner/terminal/opener': 'call Open_terminal_with_q_quit()'
                                    \, 'runner/terminal/into': 1
                                    \}


" this doesn't work properly for now
function! Open_terminal_with_q_quit()
    execute 'vertical new'
    tnoremap <buffer> <silent> q <C-w>q
    redraw
endfunction


au! FileType quickrun set nospell
nmap <leader>q <esc>:QuickRun<CR>
