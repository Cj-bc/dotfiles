let g:openbrowser_browser_commands = [
                                     \ {"name": "w3m",
                                     \  "args": "tmux split-window 'w3m {uri}'"},
                                     \ {"name": "open",
                                     \  "args": ["open -a /Applications/Google Chrome.app", "{uri}"]}
                                     \]
