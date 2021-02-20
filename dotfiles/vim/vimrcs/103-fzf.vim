nmap <C-p> :Files<CR>
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

let s:ghqRoot = system("ghq root") . "/github.com/Cj-bc/dotfiles/dotfiles"
let g:blog_dir = substitute(system("ghq list -p Cj-bc/blog"), '[\n\r]', '', 'g')
command Blog call <SID>OpenBlogEntry()

function! <SID>OpenBlogEntry()
  exec "Files ".g:blog_dir
  exec "lcd ".expand(g:blog_dir)
endfunction

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
    \ call fzf#run(fzf#wrap({
    \   'source': 'ghq list',
    \   'sink': function('<SID>CdFind')}))
endif
" }}}


" :Branch -- switch branches {{{
" This require Figitive installed
function! <SID>GitSwitch(branch)
  exec 'Git switch' a:branch
endfunction

" list branches
function! <SID>GitBranch()
  let s:git_dir = substitute(FugitiveGitDir(), '.git$','', '')
  return map(systemlist('cd '.s:git_dir.';git branch -a')
         \  , {_, val -> substitute(val, '^\*\= \+\| *-> .\+$', '', 'g')})
endfunction

command Branch
      \ call fzf#run(fzf#wrap({
                    \ 'source': <SID>GitBranch(),
                    \ 'sink': function('<SID>GitSwitch'),
                    \ 'dir': substitute(FugitiveGitDir(), '.git$','', ''),
                    \ }))
" }}}
