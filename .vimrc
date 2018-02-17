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
Plugin 'Twitvim/twitvim'
Plugin 'tyru/open-browser.vim' " dependency of TweetVim
Plugin 'basyura/twiBill.vim'  " dependency of TweetVim
Plugin 'mattn/webapi-vim'  " dependency of TweetVim (optional)
Plugin 'basyura/TweetVim'

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



" ================== Plugin configs ========================

" -------- TwitVim
let twitvim_browser_cmd = 'open'
let twitvim_force_ssl = 1
let twitvimount = 40

" =================== Reference URLs ===================================
" ==                                                                  ==
" == http://vimblog.hatenablog.com/entry/vimrc_key_mapping            ==
" ==     -> difference between key-bind commands                      ==
" == http://d.hatena.ne.jp/hyuki/20130714/vim                         ==
" ==     -> time-insert key-bind                                      ==
" == https://qiita.com/annyamonnya/items/97c5cb0cfa414b3592d7         ==
" ==     -> Cheat sheet of strftime() function                        ==
" == https://qiita.com/_snow_narcissus/items/f1633ecc40814acca4cf     ==
" ==     -> Automatically insert the pair                             ==
" == http://vimblog.hatenablog.com/entry/vimrc_set_tab_indent_options ==
" == 	 -> settings related with <tab>				                          ==
" ==                                            								      ==
" ==     		http://vim-jp.org                                         ==
" ==               VIM Japanese community site                        ==
" ==                                                                  ==
" ==          https://gitter.im/vim-jp/reading-vimrc                  ==
" ==               .vimrc Reading Community chat                      ==
" ======================================================================
