#!/bin/sh

if [ 2 -ne $# -a 3 -ne $# ]; then
    echo
    echo "Usage $0 <pattern> <file-pattern> [<dir>]"
    echo
    exit
fi

PATTERN=$1
shift
FILES=$1
shift
DIR=$1

if [ -z "$DIR" ]; then
    DIR="."
fi

find $DIR -name "$FILES" | xargs grep --color "$PATTERN"

