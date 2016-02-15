#!/bin/sh
# The CI server should run this script to verify any version

set -e
git clean -dfx

make -k check docs test
