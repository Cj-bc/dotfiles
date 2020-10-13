let g:github_issues_no_omni = 1 " set anything to disable omnifunc 
if filereadable(expand('~/.vimrc.private')) " source access token
  source ~/.vimrc.private
endif
