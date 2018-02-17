cat << _EOT_
 _|      _|                                            _|  _|      _|_|            
   _|  _|              _|_|_|  _|_|      _|_|      _|_|_|        _|      _|    _|  
     _|    _|_|_|_|_|  _|    _|    _|  _|    _|  _|    _|  _|  _|_|_|_|  _|    _|  
     _|                _|    _|    _|  _|    _|  _|    _|  _|    _|      _|    _|  
     _|                _|    _|    _|    _|_|      _|_|_|  _|    _|        _|_|_|  
                                                                               _|  
                                                                           _|_|   

                                        ____  _  ___  ____     ____ ___  
                                        |     |  |__] |        [__  |  \ 
                                        |___ _| .|__] |___ ___ ___] |__/ 

_EOT_

export PS1="<X_X>:\W \$"  # setting the prompt
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

# ------------ networks
alias airport='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport'
alias wport='networksetup -setairportpower' # usage: wport <device> <on/off>
alias wifi='networksetup -setairportnetwork' # usage: wifi <device> <SSID> <password>
alias google='[ $# -ne 0 ] && w3m thhps://www.google.com/search?q=${@// /+}  || w3m google.com' #usage: google [word]


shopt -s xpg_echo



# =========================== Reference =========================================
# ==                                                                           == 
# ==  https://qiita.com/kawaz/items/65cdbeaa739c4e6b7776                       ==
# ==      -> setting of PS4                                                    ==
# ==
# ===============================================================================
