#!/bin/sh

cd $HOME/workspace/music

pw-jack supernova -u 57110 &
supernova_pid=$!

pw-jack clj -M:repl/rebel
kill $supernova_pid
