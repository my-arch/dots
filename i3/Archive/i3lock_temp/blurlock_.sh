#!/bin/bash

# Dependencies:
# imagemagick
# i3lock
# scrot (optional but default)

IMAGE=~/.config/i3/i3lock_temp/dora_lock.png
IMAGE1=~/.config/i3/i3lock_temp/i3lock1.png
IMAGE2=~/.config/i3/i3lock_temp/i3lock2.png
IMAGE3=~/.config/i3/i3lock_temp/i3lock3.png

# Get the screenshot, add the blur and lock the screen with it
scrot $IMAGE1

# Set brightness and contrast
convert -brightness-contrast -30x-50 $IMAGE1 $IMAGE1

# Scale down and up to blur image
#convert -scale 10% -scale 1000% $IMAGE1 $IMAGE1

#convert $IMAGE1 -blur 5x6 $IMAGE1

convert -resize 10% -resize 1000% $IMAGE1 $IMAGE1
#convert -resize 25% -filter Gaussian -resize 400% $IMAGE1 $IMAGE1

## For dual monitor
#convert $IMAGE1 -crop 1920x1080+0+0 $IMAGE2
#composite -gravity center $IMAGE $IMAGE2 $IMAGE2
#
#convert $IMAGE1 -crop 1920x1080+1920+0 $IMAGE3
#composite -gravity center $IMAGE $IMAGE3 $IMAGE3
#
#convert $IMAGE2 $IMAGE3 +append $IMAGE1

# For single monitor
composite -gravity center $IMAGE $IMAGE1 $IMAGE1
i3lock -i $IMAGE1

rm $IMAGE1 $IMAGE2 $IMAGE3
