#!/bin/sh

COMMAND=$1

usage() {
    echo
    echo "Usage $(basename $0) [turn-hdmi-off | turn-hdmi-on | turn-screen-off"
    echo "                     | turn-vga-on | turn-vga-off | reset-dual"
    echo "                     | set-brightness <0..100> ]"
    echo
}

if [ -z "$COMMAND" ]; then
    usage
    exit 1
fi

setup_screen_names() {
    XRANDR=$(xrandr)
    HDMI=$(expr "$XRANDR" : '.*\(HDMI[0-9]*\) connected')
    LVDS=$(expr "$XRANDR" : '.*\(LVDS[0-9]*\) connected')
    VGA=$(expr "$XRANDR" : '.*\(VGA[0-9]*\) connected')
    echo "HDMI output: $HDMI"
    echo "LVDS output: $LVDS"
    echo "VGA  output: $VGA"
}

case $COMMAND in
    turn-hdmi-off)
        setup_screen_names
        xrandr --output $HDMI --off
        ;;
    turn-hdmi-on)
        setup_screen_names
        xrandr --output $HDMI --auto --right-of $LVDS --primary
        ;;
    turn-vga-off)
        setup_screen_names
        xrandr --output $VGA --off
        ;;
    turn-vga-on)
        setup_screen_names
        xrandr --output $VGA --auto --right-of $LVDS --primary
        ;;
    reset-dual)
        setup_screen_names
        xrandr  --output $LVDS --auto --output $HDMI --auto --right-of $LVDS --primary
        ;;
    turn-screen-off)
        xset dpms force off
        ;;
    set-brightness)
        xbacklight -set $2
        ;;
    *)
        echo
        echo "$COMMAND is not a valid command"
        echo
        exit 1
esac
