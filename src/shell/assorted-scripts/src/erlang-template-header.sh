#!/bin/sh
#
# Generates an erlang header file template

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

FILENAME=`basename $DEST`
UPCASE_NAME=`echo ${FILENAME%.hrl} | tr '[:lower:]' '[:upper:]'`

sed -e "s/@@UPCASE_NAME@@/$UPCASE_NAME/g" \
    < $TEMPLATES/erlang-header.template \
    > $DEST

echo
echo "Created $DEST!"
echo
