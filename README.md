# music

My music explorations in Clojure and Overtone.

## running the workstation

* Ensure `pipewire` is running to serve as our JACK server
* `lein install` latest overtone source (0.10.6).
* Start supernova synth engine
```
# automatically connect jack ports
export SC_JACK_DEFAULT_INPUTS="system:capture_1,system:capture_2"
export SC_JACK_DEFAULT_OUTPUTS="system:playback_1,system:playback_2"

supernova -u 57110
```
* In Emacs:
** Start emacs
** jack in normally
** evaluate all of `music/core.clj`
** you should be connected and ready to go!
