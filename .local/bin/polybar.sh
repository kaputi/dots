#!/bin/sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

primary=$(xrandr -q | grep "primary" | cut -d ' ' -f1)

echo $primary

secondary=$(xrandr -q |grep " connected"| grep -v "primary" | cut -d ' ' -f1)

export PRIMARYMONITOR=$primary
export SECONDARYMONITOR=$secondary

# Launch polybar
#polybar secondary &
polybar primary &
