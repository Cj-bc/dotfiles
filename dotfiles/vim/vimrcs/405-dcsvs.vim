let g:DCSVS_load_dragvisuals = 1
let g:DCSVS_load_betterdigraphs_utf8 = 1

"" ------- DVB {{{2
vmap  <expr>  <LEFT>   DVB_Drag('left')
vmap  <expr>  <RIGHT>  DVB_Drag('right')
vmap  <expr>  <DOWN>   DVB_Drag('down')
vmap  <expr>  <UP>     DVB_Drag('up')
vmap  <expr>  D        DVB_Duplicate()
" }}}
