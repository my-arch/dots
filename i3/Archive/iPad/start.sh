#!/bin/bash

# =============================================
# This file is to allow vnc for ufw and
# to create a virtual screen using xrandr
# and clip the screen to right using x11vnc.
# Install: x11vnc
# Before using this file, run the following
# (1024x768 is the resolution for iPad)
# (1366x768 is the resolution for laptop)
# gtf 768 1024 60
# xrandr --newmode "768x1024_60.00"  65.13  768 816 896 1024  1024 1025 1028 1060  -HSync +Vsync
# xrandr --addmode VIRTUAL1 768x1024_60.00
#
# https://rob.salmond.ca/tablet-as-external-monitor-with-i3wm/
# https://bbs.archlinux.org/viewtopic.php?id=191555
# =============================================

ufw allow vnc
# xrandr --newmode "768x1024_60.00"  65.13  768 816 896 1024  1024 1025 1028 1060  -HSync +Vsync
#xrandr --output VIRTUAL1 --mode 768x1024_60.00 --left-of eDP1
xrandr --output VIRTUAL1 --mode 768x1024_60.00 --right-of eDP1
x11vnc -clip 768x1024+1367+0
