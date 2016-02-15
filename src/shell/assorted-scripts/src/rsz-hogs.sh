#!/bin/sh

ps axo rss,pid,comm | sort -n
