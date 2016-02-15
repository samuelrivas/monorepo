#!/bin/sh
#
# Usage: send-email.sh <to> <subject>
#
# Reads standard input and sends it in an email

set -e

TO=$1
SUBJECT=$2

usage() {
    echo
    echo "Usage: `basename $0` <to> <subject>"
    echo
}

fail() {
    exit 1
}

if [ -z $TO ]; then
    usage
    fail
fi

if [ -z "$SUBJECT" ]; then
    usage
    fail
fi

## For now, get the password from a file
## The config file must define PASSWORD, EMAIL, and ACCOUNT
## Normally EMAIL == ACCOUNT@gmail.com
. $HOME/priv/send-email.conf

sendEmail \
    -f "$EMAIL" \
    -t "$TO" \
    -u "$SUBJECT" \
    -s smtp.gmail.com:587 \
    -o tls=yes \
    -xu samuelrivas \
    -xp "$PASSWD"
