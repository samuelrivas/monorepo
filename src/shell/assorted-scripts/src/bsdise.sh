#!/bin/sh
#
# Adds a BSD header to an erlang file

set -e

SCRIPT_HOME="$( cd "$( dirname "$0" )" && pwd )"
TEMPLATES="$SCRIPT_HOME/assorted-scripts/templates"
DEST=$1

if [ -z "$DEST" ]; then
    echo
    echo "usage: `basename $0` <file.hrl>"
    echo
    exit 1
fi

YEAR=`date +%Y`
TEMP=/tmp/`basename $DEST.bsdise`

cp $DEST $TEMP
sed -e "s/@@YEAR@@/$YEAR/g;s/@@COMM@@/%%%/g" \
    < $TEMPLATES/bsd-licence.template \
    > $DEST

cat $TEMP >> $DEST

echo
echo "modified $DEST!"
echo
