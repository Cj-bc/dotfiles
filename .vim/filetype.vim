	" my filetype file
	if exists("did_load_filetypes")
	  finish
	endif
	augroup filetypedetect
	  au! BufRead,BufNewFile *.applescript,*.scpt	setfiletype apples
	  au! BufRead,BufNewFile *.note	setfiletype note
	  au! BufRead,BufNewFile *.dia setfiletype diary
    
	augroup END

