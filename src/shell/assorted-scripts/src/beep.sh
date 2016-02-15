#!/bin/sh

# Bash's and dash's builtin echos interpret flags differently, so we use the
# actual binary hoping for more consistent behaviour across systems (since some
# systems cheat and symlink /bin/sh to bash whereas others are a bit more
# civilised and symlink it to dash
"$(which echo)" -n -e '\a'
