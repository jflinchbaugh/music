#!/bin/sh

cd $HOME/workspace/music

# start qjackctl to get things wired up

# automatically connect jack ports
export SC_JACK_DEFAULT_INPUTS="system:capture_1,system:capture_2"
export SC_JACK_DEFAULT_OUTPUTS="system:playback_1,system:playback_2"

#scsynth -u 57110
pw-jack supernova -u 57110
#supernova -u 57110
#supernova_pid=$!

#pw-jack clj -M:repl/cider-refactor
#kill $supernova_pid
