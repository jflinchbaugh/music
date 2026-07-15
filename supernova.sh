#!/bin/sh

cd $HOME/workspace/music

pw-jack supernova -u 57110 &
supernova_pid=$!

java -version 
pw-jack clj -M:nrepl
kill $supernova_pid
