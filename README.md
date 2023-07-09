# music

My music explorations in Clojure and Overtone.

## running the workstation

* Ensure `pipewire` is running to serve as our JACK server
* `lein install` latest overtone source (0.10.6).
* Start supernova synth engine: ./supernova.sh
* In Emacs:
** Start emacs
** jack in normally
** evaluate all of `music/core.clj`
** you should be connected and ready to go!
