#!/bin/sh

for pid in $(pidof picom); do
    if [ $pid != $$ ]; then
        kill -9 $pid
    fi
done

# funciona
# monnitor left
# xrandr --output eDP1 --primary --mode 3000x2000 --pos 3840x0 --rotate normal --scale 1x1 --output DP2 --mode 1920x1080 --pos 0x0 --rotate normal --scale 2x2
# xrandr --output eDP-1-1 --primary --mode 3000x2000 --pos 3840x-30 --rotate normal --scale 1x1 --output DP-1-2 --mode 1920x1080 --pos 0x0 --rotate normal --scale 2x2

# monitor up
# xrandr --output eDP-1-1 --primary --mode 3000x2000 --pos 0x0 --rotate normal --scale 1x1 --output DP-1-2 --mode 1920x1080 --pos 0x-2000 --rotate normal --scale 1.75x1.75
xrandr --output eDP-1-1 --primary --mode 3000x2000 --pos 0x0 --rotate normal --scale 1x1 --output DP-1-2 --mode 1920x1080 --pos 0x-2000 --rotate normal --scale 5x5


# xrandr --output eDP1 --primary --mode 3000x2000 --pos 3840x0 --rotate normal --scale 1x1 --output DP2 --mode 1920x1080 --pos 0x0 --rotate normal --scale 2x2
# xrandr --output eDP1 --primary --mode 3000x2000 --pos 0x0 --rotate normal --scale 1x1 --output DP2 --mode 1920x1080 --pos 3000x-30 --rotate normal --scale 2x2

picom
