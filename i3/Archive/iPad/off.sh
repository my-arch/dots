#!/bin/bash

# =============================================
# This file is to deny vnc for ufw and
# to disconnect VIRTUAL1 screen
# =============================================

xrandr --output VIRTUAL1 --off
ufw deny vnc
