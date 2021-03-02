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

" Xdg2path: Convert XDG variables to real path
def Xdg2path(xdgName: string): list<string>
  const xdgDefault = {
          "XDG_DATA_HOME":   expand("$HOME/.local/share"),
          "XDG_CONFIG_HOME": expand("$HOME/.config"),
          "XDG_DATA_DIRS":   "/usr/local/share/:/usr/share/",
          "XDG_CONFIG_DIRS": "/etc/xdg",
          "XDG_CACHE_HOME": expand("$HOME/.cache"),
          "XDG_RUNTIME_DIR": "",
                     }
  const envValue = getenv(xdgName)
  if has_key(environ(), xdgName) && envValue != ""
    return split(envValue, ":")
  endif

  # If that variable is either not defined or empty,
  # use default value
  return split(xdgDefault[xdgName], ":")
enddef

" FzfXdgRun: fizzy find files under specified XDG specified directory
def <SID>FzfXdgRun(xdgName: string): void
  fzf#run(fzf#wrap({'source': "find -L " .. join(Xdg2path(xdgName), " ")
                                         .. " -mindepth 1 "
                                         .. "\\( -path '*/\\.*' -o -fstype 'sysfs' "
                                         .. "-o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune "
                                         .. "-type f -print -o -type l -print 2> /dev/null"}))
enddef

command Xdg
      \ call fzf#run(fzf#wrap({
                    \ 'source': keys(xdgDefault),
                    \ 'sink': function("<SID>FzfXdgRun"),
                    \ }))
defcompile
