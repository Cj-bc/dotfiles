" functions below are not used for now
" let s:translate_lang_shortennames = { "e": "en",
"                                    \  "j": "ja"
"                                    \}
" function s:translate_detect_lang(a:shorten)
"   if ! has_key(s:translate_lang_shortennames, a:shorten)
"     return ''
"   endif
"
"   return s:translate_lang_shortennames[a:shorten]
" endfunction
vnoremap <leader>tlej :TranslateVisual en:ja<CR>
vnoremap <leader>tlje :TranslateVisual ja:en<CR>
nnoremap <leader>tlej :Translate en:ja<CR>
nnoremap <leader>tlje :Translate ja:en<CR>
nnoremap <leader>tlc :TranslateClear<CR>
