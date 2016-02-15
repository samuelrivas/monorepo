#!/bin/sh

if [ $# -ne 1 ]; then
    echo
    echo "Usage: `basename $0` <number>"
    echo
    exit 1
fi

echo "ibase=16;$1" | bc
