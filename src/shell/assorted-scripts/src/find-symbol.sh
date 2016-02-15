#!/bin/sh
#
# Usage: find-symbol.sh <symbol> <list-of-paths>

if [ $# -lt 2 ]; then
    echo
    echo "Usage: `basename $0` <symbol> <list-of-paths>"
    echo
    exit 1
fi

SYMBOL=$1

shift

for i in `find $@ -name "*.so*" -and -type f`; do
    SYMBOLS=`nm -D $i 2> /dev/null | grep ' [T|D] ' | grep $SYMBOL`
    if [ "$SYMBOLS" ]; then
	echo "\033[32m$i\033[0m";
	echo $SYMBOLS
	echo
    fi
done
