" ================= For Vundle =============================
set modeline
set modelines=5
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
 call vundle#begin()
 " ------- Write plugins here
 " Plugin '[Github Author]/[Github repo]'
 "
 "
Plugin 'VundleVim/Vundle.vim'
" Plugin 'Twitvim/twitvim'
" Plugin 'tyru/open-browser.vim' " dependency of TweetVim
" Plugin 'basyura/twiBill.vim'  " dependency of TweetVim
Plugin 'mattn/webapi-vim'  " send http request from vim
" Plugin 'basyura/TweetVim'
Plugin 'junkblocker/patchreview-vim' " dependency of vim-codereview
Plugin 'codegram/vim-codereview'  " code reviewer for Github's Pull-Request
Plugin 'Shougo/unite.vim' " unite.vim -- a breaking User Interface
Plugin 'kmnk/vim-unite-giti' " add Git source for unite.vim
Plugin 'mattn/emoji-vim' " type Emoji in Vim!!
Plugin 'Shougo/vimproc.vim' " optionally dependency of github-complete.vim
Plugin 'rhysd/github-complete.vim' " complete github's username, repository name, etc
Plugin 'jaxbot/github-issues.vim' " deal with github issues
Plugin 'freitass/todo.txt-vim' " vim plugin for todo.txt
Plugin 'vim-scripts/bats.vim' " just a hilight for Bats
call vundle#end()
filetype plugin indent on

" ================ Generals =================================
set cursorline
set number
syntax on


set expandtab " replace <tab> with <space>
set tabstop=2 " width <tab> hold
set shiftwidth=2 " width auto-indent
	" highlight ZENKAKU-SPACE not to miss them
highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=#666666
au BufNewFile,BufRead * match ZenkakuSpace /　/
  " display unvisible letter
  "   tab: >>-
set list
set listchars=tab:>-,trail:-,extends:>,precedes:<,nbsp:%

au BufNewFile *.dia 0r $HOME/.vim/template/diary.txt
au BufNewFile pack.mcmeta 0r $HOME/.vim/template/pack.mcmeta

" ================== Key binds ==============================
	" jj to exit insert-mode
inoremap <silent> jj <ESC> 
	" <C-o><C-o> to print time in normal-mode
inoremap <C-o><C-o> <C-r>=strftime('(%T %p)') <LF>
	" <C-o>d to print data in normal-mode
nnoremap <C-o>d <ESC>i<C-r>=strftime('< %F >')<LF><LF>
	" Reload .vimrc by pushing <C-o>i
nnoremap <C-o>i <ESC>:source ~/.vimrc<LF><LF>

	" Automatically insert the pair
inoremap [ []<left>
inoremap ( ()<left>
inoremap { {}<left>
inoremap " ""<left>

" key bind for vim-codereview
nnoremap ,cc <ESC>:CodeReviewCommentChange <LF>
nnoremap ,ch <ESC>:CodeReviewComment <LF>
nnoremap ,cr <ESC>:CodeReviewReloadComments <LF>



" ================== Plugin configs ========================

" " -------- TwitVim
" let twitvim_browser_cmd = 'open'
" let twitvim_force_ssl = 1
" let twitvimount = 40

" -------- github-completion
augroup config-github-complete
  autocmd!
  autocmd FileType gitcommit setl omnifunc=github_complete#complete
  autocmd FileType markdown setl omnifunc=github_complete#complete
  autocmd FileType conf  setl omnifunc=github_complete#complete
  autocmd FileType gfimarkdown setl omnifunc=github_complete#complete
augroup END

" ------- github-issues.vim
let g:github_issues_no_omni = 1 " set anything to disable omnifunc 
if filereadable(expand('~/.vimrc.private')) " source access token
  source ~/.vimrc.private
endif

" ------- netrw
" usage:
"   i --- change view mode
"   v --- open file on the side
"   o --- open file up to the newtw
let g:netrw_liststyle = 3 " force tree view (can be toggled by 'i' button)
let g:netrw_altv = 1 " open files on the right side when 'v' is pushed



" =================== Reference URLs =========================================
" ==                                                                        ==
" == http://vimblog.hatenablog.com/entry/vimrc_key_mapping                  ==
" ==     -> difference between key-bind commands                            ==
" == http://d.hatena.ne.jp/hyuki/20130714/vim                               ==
" ==     -> time-insert key-bind                                            ==
" == https://qiita.com/annyamonnya/items/97c5cb0cfa414b3592d7               ==
" ==     -> Cheat sheet of strftime() function                              ==
" == https://qiita.com/_snow_narcissus/items/f1633ecc40814acca4cf           ==
" ==     -> Automatically insert the pair                                   ==
" == http://vimblog.hatenablog.com/entry/vimrc_set_tab_indent_options       ==
" == 	 -> settings related with <tab>				                                ==
" == http://blog.tojiru.net/article/234400966.html                          ==
" ==  -> settings of netrw config                                           ==
" == http://blog.remora.cx/2011/08/display-invisible-characters-on-vim.html ==
" ==  -> visible unvisible letters (set list)                               ==
" ==                                                                        ==
" ==     		http://vim-jp.org                                               ==
" ==               VIM Japanese community site                              ==
" ==                                                                        ==
" ==          https://gitter.im/vim-jp/reading-vimrc                        ==
" ==               .vimrc Reading Community chat                            ==
" ============================================================================
