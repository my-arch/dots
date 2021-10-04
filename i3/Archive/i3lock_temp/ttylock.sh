#!/bin/bash

xfce4-terminal -e 'tty-clock -c' &
sleep 0.2

i3-msg fullscreen

i3lock -n; i3-msg kill
