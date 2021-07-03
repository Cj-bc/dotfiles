# Reference
# https://qiita.com/tayusa/items/3f44673c1493ae352fc0


declare -A menu=(
    ['abort']='true'
    ['lock']='dm-tool lock'
    ['poweroff']='systemctl poweroff'
    ['reboot']='systemctl reboot'
  )

map() {
  local f=$1
  while read line; do
    eval "echo $(f $line)"
  done < <(cat -)
}

if [[ $# -eq 0 ]]; then
  echo "${!menu[@]}" | tr ' ' '\n'
elif [[ $# -eq 1 ]]; then
  case $1 in
    "poweroff"|"reboot" )
      echo -en "\0prompt\x1f$1?\n"
      echo -en "yes\0info\x1f$1\n"
      echo -en "no\0info\x1f$1\n"
      ;;
    "yes" ) eval ${menu[$ROFI_INFO]} ;;
    "no" ) true ;;
    * ) eval "${menu[$1]}"
  esac
fi
