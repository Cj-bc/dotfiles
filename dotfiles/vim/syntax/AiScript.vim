" TODO: '#'はキーワード文字ではないのでmatchにしている、後でキーワード文字に追加して'keyword'にしたほうがいいかも
syntax match AiMetaHeader '###'

" Primitive types {{{
syn match AiStr +"[^"]\{-}"+
syn match AiNumber "\<[0-9]\+\>"
syn match AiBool "\<\(yes\|no\)\>"
syn region AiArr contains=AiBool,AiStr,AiNumber start="\[" end="\]"
syn keyword AiNull "_"

" Function {{{2
syn match AiFuncDef nextgroup=AiBlock contains=AiFuncName,AiFuncArgName "\s*\w\+@(\%(\w\+\)\(\s*,\%(\w\+\)\)*)"
syn match AiFuncName nextgroup=AiFuncDef "^\s*\w\+"
syn match AiFuncArgName "(\w\+, "
" }}}


" Object {{{2
" TODO: foldつける
syntax region AiBlock contains=AiObjectKeyword,AiBool,AiNumber,AiStr start='{' end='}'
syntax match AiObjectKeyword containedin=AiBlock contained '\w*\ze: '
" 2}}}

syn cluster AiPrimitives contains=AiStr,AiNumber,AiBool,AiArr,AiNull,AiVariable
" }}}

syn match AiVariableDef nextgroup=@AiPrimitives contains=AiVariableName "^\s*#\w\+ = "
syn match AiVariableName contained "#\zs\w\+"

" Highlights {{{
hi link AiMetaHeader Keyword
hi link AiStr String
hi link AiBool Boolean
hi link AiNumber Number
hi link AiObjectKeyword Identifier
hi link AiFuncDef Keyword
hi link AiFuncName Function
hi link AiVariableName Identifier
