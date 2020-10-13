let g:tweetvim_silent_say = 1
let g:tweetvim_display_source = 1
nnoremap tww <esc>:TweetVimCommandSay<CR>
nnoremap twl <esc>:split +TweetVimHomeTimeline<CR>
nnoremap twt <esc>:split +Unite\ tweetvim/account<CR>
nnoremap twm <esc>:split +TweetVimMentions<CR>
nnoremap tws <esc>:TweetVimSay<CR>
nnoremap twme <esc>:split +TweetVimUserTimeline\ cj_bc_sd<CR>
