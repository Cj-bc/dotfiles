cat << _EOT_
===               ===  ===  ======      =========  ===        =========     ======      ======  
 ===     ===     ===    =   ===  ===    ===        ===        ===         ===    ===  ===    ===
  ===   =====   ===     =   ===   ===   =========  ===        =========      ===         ===    
   === === === ===      =   ========    ===        ===        ===              ===         ===  
    =====   =====       =   ===   ===   ===        ===        ===         ===   ===   ===   === 
     ===     ===       ===  ===    ===  =========  =========  =========     ======      ======  

                                             /\____/\ \   ____  _  ___  ____     ____ ___    /
                                            | ^<>^  |  \  |     |  |__] |        [__  |  \  /
                                            |=======|   \ |___ _| .|__] |___ ___ ___] |__/ /
                
_EOT_

git_branch() {
  echo $(git branch 2>/dev/null | sed -ne "s/^\* \(.*\)$/\1/p")
}
function torf() {
  return $stat
}
export PS1="\`stat=\"\$?\";echo -n '\$?is'\$stat;[ -e ./.git ] && echo [\$(git_branch)];torf\`\u@\h :\W\\$ " # \ before $value is really important!
export PS4='+(${BASH_SOURCE}:${LINENO}): ${FUNCNAME:+$FUNCNAME(): }' # setting debug prompt

# ================================= Alias ====================================
alias ls='ls -G'
alias la='ls -a'
alias ll='ls -l'
alias tree='tree -C'
alias bashrc='source ~/.bashrc'
alias back='cd $OLDPWD'

# ------------ apps
alias dia='vim ~/.diary/$(date "+%Y/%m/%d").md'
alias td='todo.sh'

# ------------ networks
alias airport='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport'
alias wport='networksetup -setairportpower' # usage: wport <device> <on/off>
alias wifi='networksetup -setairportnetwork' # usage: wifi <device> <SSID> <password>


shopt -s xpg_echo

export PATH=$PATH:~/.nodebrew/current/bin

# ================== setting for todo-txt
source /usr/local/etc/bash_completion.d/todo_completion
complete -F _todo td # set todo.sh completion for td alias

# =========================== Reference =========================================
# ==                                                                           == 
# ==  https://qiita.com/kawaz/items/65cdbeaa739c4e6b7776                       ==
# ==      -> setting of PS4                                                    ==
# == http://note.kurodigi.com/bashprompt-customize/                            ==
# ==      -> color of PS1(echo command)                                        ==
# == https://news.mynavi.jp/article/20090909-bash/                             ==
# ==      -> change PS1 when $? -ne 0                                          ==
# == https://qiita.com/iwazer/items/5f57a80b8aac0f4e9839                       ==
# ==      -> add git-branch in PS1                                             ==
# ===============================================================================
