#!/bin/sh

for i in `git branch -r | sed -ne 's| *tags/v\(.*\)$|\1|p'`; do
    echo "Creating $i"
    git tag $i tags/v$i;
done

echo
echo "Done"
echo