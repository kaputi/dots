#!/bin/bash

delay=60

while true; do

  # xdotool key Escape
  # xdotool key a
  # xdotool key alt

  # xdotool mousemove_relative 50 50 mousemove_relative -- -50 -50
  xdotool mousemove_relative 1 0 mousemove_relative -- -1 -0

  echo $(date +"%T")
  sleep $delay
done
