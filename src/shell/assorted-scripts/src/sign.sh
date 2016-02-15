#!/bin/sh
#
# Usage: sign.sh
#
# Reads text from stdin and outpus a copy with an armored signature

gpg --armor --clearsign

