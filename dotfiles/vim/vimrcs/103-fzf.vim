nmap <C-p> :Files<CR>
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

let s:ghqRoot = system("ghq root") . "/github.com/Cj-bc/dotfiles/dotfiles"
command Config call fzf#vim#files("/Users/cj-bc/ghq/github.com/Cj-bc/dotfiles/dotfiles")
command Blog call fzf#vim#files(g:blog_dir)
