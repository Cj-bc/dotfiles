OREDAYO4M_EXECUTABLE="~/Downloads/Linux/Oredayo4M.x86_64"

## If you run Oredayo4M in different X server, You should set
## its number to TARGET_DISPLAY
# TARGET_DISPLAY=:1

if [[ $(ps x | grep "Oredayo4M" | wc -l) -lt 2 ]];then
    $OREDAYO4M_EXECUTABLE
fi

winId=$(xwininfo -name "Oredayo4M" | awk '/^xwininfo:/ {print $4}')

if ! [[ $winId =~ ^0x[0-9a-f]{7}$ ]]; then
    echo "Couldn't get window id for Oredayo4M" >&2
    exit
fi

# ffmpeg -i ~/Picture/wallpaper/rizuNM_rollcake-with-kirby.jpeg \
ffmpeg -f lavfi -i "color=c=dimgray:1920x1080" \
       -f x11grab -window_id $winId -i ${TARGET_DISPLAY:-$DISPLAY} \
       -filter_complex "
       [1:v]chromakey=color=#00FF00:similarity=0.15,
		scale=w=2*iw:h=2*ih[avatar];
       [0:v][avatar]overlay=x=(main_w/2)-(overlay_w/2):
			    y=main_h-overlay_h
			" \
	-pix_fmt yuv420p -f v4l2 /dev/video4
