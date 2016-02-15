#!/bin/sh

if [ $# -ne 2 ]; then
    echo
    echo "Usage $0 <vsize> <hsize>"
    echo
    exit 1
fi

VSIZE=$1
HSIZE=$2

gconftool-2 --type=int --set /apps/compiz-1/general/screen0/options/vsize $VSIZE
gconftool-2 --type=int --set /apps/compiz-1/general/screen0/options/hsize $HSIZE
