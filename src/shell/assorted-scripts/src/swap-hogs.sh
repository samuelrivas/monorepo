#!/bin/sh
# Mostly stolen from (i.e. thanks to) Erik Ljungstrom:
#   http://northernmost.org/blog/find-out-what-is-using-your-swap/

SUM=0
for DIR in $(find /proc/ -maxdepth 1 -type d | egrep "^/proc/[0-9]") ; do
    PID=$(echo $DIR | cut -d / -f 3)
    PROGNAME=$(ps -p $PID -o comm --no-headers)
    for SWAP in $(grep Swap $DIR/smaps 2>/dev/null| awk '{ print $2 }')
    do
        SUM=$(($SUM+$SWAP))
    done
    echo "$SUM $PID $PROGNAME"
    SUM=0
done | grep -v ^0 | sort -n
