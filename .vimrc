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
Plugin 'itchyny/calendar.vim' " view calender from vim
Plugin 'itchyny/screensaver.vim' " screen saver
Plugin 'kana/vim-textobj-user' " base plugin for textobjects
Plugin 'kana/vim-operator-user' " base plugin for vim operators
Plugin 'rhysd/vim-operator-surround' " Vim operator mapping to enclose text objects with surrounds like paren, quote and so on.
Plugin 'scrooloose/vim-slumlord' " Inline previews for Plantuml sequence diagrams
Plugin 'aklt/plantuml-syntax' " syntax file for Plant UML
Plugin 'kannokanno/previm' " previewer for markdown in browser
Plugin 'VincentCordobes/vim-translate' " Translater in Vim (require: translate-shell)
Plugin 'vimwiki/vimwiki' " Personal Wiki for Vim
Plugin 'mhinz/vim-startify' " The fancy start screen for Vim
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

  " iskeyword define which letter is included as word
au BufNewFile,BufRead *.sh set iskeyword=.,@,48-57,_,192-255
au BufNewFile,BufRead *.bats set dictionary=$HOME/.vim/dictionary/bats.dict

au BufNewFile *.dia 0r $HOME/.vim/template/diary.txt
au BufNewFile pack.mcmeta 0r $HOME/.vim/template/pack.mcmeta
au BufNewFile *.bats 0r $HOME/.vim/template/bats.bats


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

" key binds for operator mappings 
nnoremap <silent>sa <Plug>(operator-surround-append)
nnoremap <silent>sd <Plug>(operator-surround-delete)
nnoremap <silent>sr <Plug>(operator-surround-replace)

" show hint for <C-x> completion
  " dictionary of input key
  let s:compl_key_dict = {
    \ char2nr("\<C-l>"): "\<C-x>\<C-l>",
    \ char2nr("\<C-n>"): "\<C-x>\<C-n>",
    \ char2nr("\<C-p>"): "\<C-x>\<C-p>",
    \ char2nr("\<C-k>"): "\<C-x>\<C-k>",
    \ char2nr("\<C-t>"): "\<C-x>\<C-t>",
    \ char2nr("\<C-i>"): "\<C-x>\<C-i>",
    \ char2nr("\<C-]>"): "\<C-x>\<C-]>",
    \ char2nr("\<C-f>"): "\<C-x>\<C-f>",
    \ char2nr("\<C-d>"): "\<C-x>\<C-d>",
    \ char2nr("\<C-v>"): "\<C-x>\<C-v>",
    \ char2nr("\<C-u>"): "\<C-x>\<C-u>",
    \ char2nr("\<C-o>"): "\<C-x>\<C-o>",
    \ char2nr('s'): "\<C-x>s",
    \ char2nr("\<C-s>"): "\<C-x>s"
    \ }

  " display message
  let s:hint_i_ctrl_x_msg = join([
    \ '<C-l>: while lines',
    \ '<C-n>: keywords in the current file',
    \ "<C-k>: keywords in 'dictionary'",
    \ "<C-t>: keywords in 'thesaurus'",
    \ '<C-i>: keywords in the current and included files',
    \ '<C-]>: tags',
    \ '<C-f>: file names',
    \ '<C-d>: difinitions or macros',
    \ '<C-v>: Vim command-line',
    \ "<C-u>: User defined completion ('completefunc')",
    \ "<C-o>: omni completion ('omnifunc')",
    \"s: Spelling suggestions ('spell')"
    \],"\n")

  function! s:hint_i_ctrl_x() abort
    echo s:hint_i_ctrl_x_msg
    let c = getchar()
    return get(s:compl_key_dict, c, nr2char(c))
  endfunction

  inoremap <expr> <C-x> <SID>hint_i_ctrl_x()
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


" ------- previm
let g:previm_open_cmd = 'open -a Chrome'
augroup PrevimSettings
  autocmd!
  autocmd BufNewFile,BufRead *.{.md,mdwn,mkd,mkdn,mark*} set filetype=markdown
augroup END

" ------- netrw
" usage:
"   i --- change view mode
"   v --- open file on the side
"   o --- open file up to the newtw
let g:netrw_liststyle = 3 " force tree view (can be toggled by 'i' button)
let g:netrw_altv = 1 " open files on the right side when 'v' is pushed

" ------ UML view in chrome
au FileType plantuml command! OpenUML :!open -a % 
au BufRead *.uml command! OpenUML :!open -a % 
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
" == http://koturn.hatenablog.com/entry/2018/02/10/170000                   ==
" ==  -> display hint of <C-x>                                              ==
" == http://d.hatena.ne.jp/osyo-manga/20140307/1394204974                   ==
" ==  -> documents for textobj and operators                                ==
" ==                                                                        ==
" ==     		http://vim-jp.org                                               ==
" ==               VIM Japanese community site                              ==
" ==                                                                        ==
" ==          https://gitter.im/vim-jp/reading-vimrc                        ==
" ==               .vimrc Reading Community chat                            ==
" ============================================================================
