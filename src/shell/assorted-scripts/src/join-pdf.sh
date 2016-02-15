#!/bin/sh

if [ $# -lt 3 ]; then
    cat <<EOF

Usage $(basename $0) <output-file> <input-file>-

EOF
    exit
fi

OUTPUT=$1
shift

pdftk "$@" cat output "$OUTPUT"

echo
echo "Done! You've got your new pdf file in $OUTPUT"
echo
