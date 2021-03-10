# dotfiles for Cj-bc

This is while dotfiles repository for my use.

# set up on new device

Currently, there's no automated setup script here.

```bash
$ ./init.sh
# dotfiles will be symlinked to each place. config file is 'config.txt'
# homebrew configuration ---
$ brew bundle install --file=dotfiles/brew/Brewfile  # for minimum, replace Brewfile with Brewfile-core
# Vundle & vim plugins installation ---
$ mkdir ~/.vim/bundle
$ git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
$ vim -C PluginInstall
# tmux plugin installation ---
$ git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
$ tmux
# on tmux, press <PREFIX>+I (capital "I")
```
