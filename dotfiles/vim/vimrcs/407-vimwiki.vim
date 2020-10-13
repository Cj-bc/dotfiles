au FileType vimwiki set noreadonly
let wiki_type_md = {'syntax': 'markdown', 'custom_wiki2html': '~/vimwiki/misaka_md2html.py', 'css_name': '~/vimwiki/template/style.css'}
let wiki_default = {'path': '~/vimwiki/wiki_default', 'path_html': '~/vimwiki/html/wiki_default'}
let wiki_note = {'path': '~/Documents/notes', 'path_html': '~/vimwiki/html/notes'}
let wiki_projects = {'path': '~/vimwiki/wiki_projects', 'path_html': '~/vimwiki/html/wiki_projects', 'css_name': '~/vimwiki/template/style.css'}
let wiki_tfc = { 'path': '~/Desktop/tfc/wiki', 'path_html': '~/Desktop/tfc/wiki_html/', 'css_name': '~/vimwiki/template/style.css'}
let wiki_codereading = { 'path': '~/vimwiki/wiki_codereading', 'path_html': '~/vimwiki/html/wiki_codereading'}
let wiki_howm = { 'path': howm_dir, 'path_html': howm_dir . '/html', 'syntax': 'markdown', 'css_name': '~/vimwiki/template/style.css'}
let wiki_yozakura_project = { 'path': '~/Desktop/dev/projects/yozakura-project/wiki', 'path_html': '~/Desktop/dev/projects/yozakura-project/wiki_html'}

call extend(wiki_codereading      , wiki_type_md)
call extend(wiki_yozakura_project , wiki_type_md)
call extend(wiki_note             , wiki_type_md)
call extend(wiki_default          , wiki_type_md)

let g:vimwiki_list = [wiki_default, wiki_note, wiki_projects, wiki_tfc, wiki_codereading, wiki_howm, wiki_yozakura_project]
let g:vimwiki_folding = 'expr'
" --- functions
" toggle syntax.
" source: http://tk2000ex.blogspot.com/2013/12/markdownvimwiki.html
" Vimwiki_syntax_toggle {{{2
function! Vimwiki_syntax_toggle()
  if ( &filetype == 'markdown')
    set filetype=vimwiki
  elseif ( &filetype == 'vimwiki' )
    set filetype=markdown
  endif
endfunction

" commit automatically when leaving wiki page
" dependencies: vim-fugitive
" Vimwiki_commit_automatic {{{2
function! Vimwiki_commit_automatic()
  if ( &filetype == 'vimwiki' )
    lcd `=vimwiki#vars#get_wikilocal('path')`
    if system('git status --short') != ""
      Git add *
      Git commit -m "Auto commit by Vimwiki_commit_automatic"
      redraw | echomsg 'commited.'
    else
      redraw | echomsg 'Nothing to commit.'
    endif
  endif
endfunction

" select notes dir by class name.
" Open top directory if no name is specified
" notes_select_class {{{2
function! s:notes_select_class(class_name)
  let s:wiki_num = match(g:vimwiki_list, "'path': '.*notes[^']*'")
  let s:index_file = vimwiki#vars#get_wikilocal('index') . vimwiki#vars#get_wikilocal('ext')
  if len(a:class_name) == 0
    call vimwiki#base#goto_index(s:wiki_num +1) " +1 because wiki's number start from 1, otherwize s:wiki_num start from 0
    set nofoldenable
  else
    let s:wiki_dir = vimwiki#vars#get_wikilocal('path', s:wiki_num) . a:class_name
    if isdirectory(s:wiki_dir)
      edit `=s:wiki_dir . '/' . s:index_file`
      let b:vimwiki_prev_link =  [vimwiki#vars#get_wikilocal('path', s:wiki_num) . s:index_file, [0, 0, 0, 0]]
      set nofoldenable
    else
      echohl Error | echomsg 'class: ' . a:class_name . ' is not defined.' | echohl None
    endif
  endif

endfunction
command! -nargs=? Notes :call s:notes_select_class("<args>")
" }}}

" autogroup {{{2
augroup Vimwiki_CustomCommands
  autocmd!
  autocmd BufNewFile,BufRead *.wiki nmap ,s :call Vimwiki_syntax_toggle()<CR>
  autocmd BufNewFile,BufRead *.wiki nmap vc :call Vimwiki_commit_automatic()<CR>
  autocmd QuitPre *.wiki call Vimwiki_commit_automatic()
augroup END
" }}}

