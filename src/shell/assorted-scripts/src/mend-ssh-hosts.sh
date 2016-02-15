#!/bin/sh
#
# copy-n-paste the ssh message like this:
#   mend-ssh-hosts.sh /home/samuel/.ssh/known_hosts:135

set -e

if [ -z "$1" ]; then
    echo
    echo "Usage: copy-n-paste the ssh message like this:"
    echo "    mend-ssh-hosts.sh /home/samuel/.ssh/known_hosts:135"
    echo
    exit 1
fi


FILE=`echo $1 | cut -d : -f 1`
LINE=`echo $1 | cut -d : -f 2`

sed -i.back -e "$LINE d" $FILE

echo "Deleted offending key from $FILE. Backup in $FILE.back"
