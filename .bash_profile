if [ -f ~/.bashrc ]
then
  source ~/.bashrc
fi

# for bash-completion 
if [ -f /usr/local/etc/bash_completion ]
then
  . /usr/local/etc/bash_completion
elif [ -f /usr/local/share/bash-completion/bash_completion ]
then
# for bash-completion@2
  . /usr/local/share/bash-completion/bash_completion
fi

export PATH=$PATH:~/.nodebrew/current/bin

