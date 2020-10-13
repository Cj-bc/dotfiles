augroup config-github-complete
  autocmd!
  autocmd FileType gitcommit setl omnifunc=github_complete#complete
  autocmd FileType markdown setl omnifunc=github_complete#complete
  autocmd FileType conf  setl omnifunc=github_complete#complete
  autocmd FileType gfimarkdown setl omnifunc=github_complete#complete
augroup END
