

syntax keyword list /*/
syntax keyword list /[1-9]./
syntax keyword list /[1-9][0-9]./
syntax region URI start="https://" end=","
syntax region URI start="http://" end=","
syntax region URI start="ftp://" end=","
syntax region URI start="ssh://" end=","
syntax region page p.
syntax region title start=/#/ end=/$/
syntax region emphasis start="\S\@<=\*\*\|\*\*\S\@=" end="\S\@<=\*\*\|\*\*\S\@="
syntax match list /\%(\t\| \{0,4\}\)[-*+]\%(\s\+\S\)\@=/


hi def link title htmlH1
hi def link emphasis htmlBold
hi def link list htmlTagName
