nmap <C-p> :Files<CR>
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

let s:ghqRoot = system("ghq root") . "/github.com/Cj-bc/dotfiles/dotfiles"
command Blog call fzf#vim#files(g:blog_dir)


" Ghq command {{{
" from https://techracho.bpsinc.jp/jhonda/2019_12_24/85173
function! <SID>CdFind(dir)
  let s:path = substitute(system('ghq root'), '[\r\n]', '', 'g').'/'.a:dir
  echo s:path
  exec 'lcd' s:path
  let b:git_dir = s:path
  GFiles
endfunction

if executable('ghq')
  " :Ghq command allows you fzf git repositories, then launch ':GFiles'
  command! -bang -nargs=0 Ghq
    \ call fzf#run({
    \   'source': 'ghq list',
    \   'sink': function('<SID>CdFind')})
endif
" }}}
