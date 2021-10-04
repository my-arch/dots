#!/bin/bash

# Dependencies:
# ffmpeg
# i3lock

IMAGE=~/.config/i3/i3lock_temp/dora_lock.png
IMAGE1=~/.config/i3/i3lock_temp/i3lock1.png

RES=$(xrandr | grep 'current' | sed -E 's/.*current\s([0-9]+)\sx\s([0-9]+).*/\1x\2/')

TMPBG=~/.config/i3/script/screen.png
LOCK=~/.config/i3/script/lock.png
 
ffmpeg -f x11grab -video_size $RES -y -i $DISPLAY -i $IMAGE -filter_complex \
"boxblur=5:1,overlay=(main_w-overlay_w)/2:(main_h-overlay_h)/2" -vframes 1 \
$IMAGE1 -loglevel quiet

i3lock -i $IMAGE1

rm $IMAGE1
