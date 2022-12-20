#!/bin/bash

#device=11
device="SYNA2393:00 06CB:19AC"
state=$(xinput list-props "$device" | grep "Device Enabled" | grep -o "[01]$")

echo $state

if [ $state == '1' ];then
  xinput --disable "$device"
else
  xinput --enable "$device"
fi