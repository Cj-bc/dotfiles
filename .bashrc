cat << _EOT_
 _|      _|                                            _|  _|      _|_|            
   _|  _|              _|_|_|  _|_|      _|_|      _|_|_|        _|      _|    _|  
     _|    _|_|_|_|_|  _|    _|    _|  _|    _|  _|    _|  _|  _|_|_|_|  _|    _|  
     _|                _|    _|    _|  _|    _|  _|    _|  _|    _|      _|    _|  
     _|                _|    _|    _|    _|_|      _|_|_|  _|    _|        _|_|_|  
                                                                               _|  
                                                                           _|_|   

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
export PS1="\`stat=\"\$?\";echo -n '\$?is'\$stat;[ -e ./.git ] && echo [\$(git_branch)];[ \${stat} = 0 ] && echo \[\e[33m\]\<X_X\>\[\e[0m\] || echo \[\e[31m\]\<@_@\>\[\e[0m\];torf\`:\W\\$ " # \ before $value is really important!
export PS4='+(${BASH_SOURCE}:${LINENO}): ${FUNCNAME:+$FUNCNAME(): }' # setting debug prompt

# ================================= Alias ====================================
alias ls='ls -G'
alias la='ls -a'
alias ll='ls -l'
alias tree='tree -C'
alias bashrc='source ~/.bashrc'
alias back='cd $OLDPWD'

# ------------ apps
alias siri='open /Applications/Siri.app'
alias drp='cd ~/Dropbox && ls'
alias stopwatch='utimer --stopwatch'
alias dia='vim ~/.diary/$(date "+%Y/%m/%d").md'

# ------------ networks
alias airport='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport'
alias wport='networksetup -setairportpower' # usage: wport <device> <on/off>
alias wifi='networksetup -setairportnetwork' # usage: wifi <device> <SSID> <password>
alias google='[ $# -ne 0 ] && w3m https://www.google.com/search?q=${@// /+}  || w3m google.com' #usage: google [word]


shopt -s xpg_echo



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
