#!/bin/bash

STATE=$(amixer get Master | tail -n 1 | sed -r 's/.*[^[]*\[([^]]*)\]$/\1/')
if [[ "$STATE" = "off" ]]; then
    echo "MUTE"
else
    amixer get Master | tail -n 1 | sed -r 's/[^[]*\[([^]]*)\].*$/\1/'
fi
