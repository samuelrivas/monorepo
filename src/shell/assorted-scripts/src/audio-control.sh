#!/bin/sh

COMMAND=$1

usage() {
    echo
    echo "Usage $(basename $0) [test | mute | up | down | set <N>]"
    echo
}

if [ -z "$COMMAND" ]; then
    usage
    exit 1
fi

case $COMMAND in
    test)
        pactl play-sample 0 0
        ;;
    mute)
        xrandr --output HDMI2 --off
        ;;
    turn-hdmi-on)
        xrandr --output HDMI2 --auto --right-of LVDS1
        ;;
    turn-screen-off)
        xset dpms force off
        ;;
esac
