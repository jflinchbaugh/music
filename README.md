# music

My music explorations in Clojure and Overtone.

## running the workstation

* Ensure `pipewire` is running to serve as our JACK server
* `lein install` latest overtone source (0.10.6).
* Start supernova and an nrepl: ./supernova.sh
* In Emacs:
** Start emacs
** connect to the running repl
** evaluate all of `music/core.clj`
** you should be connected and ready to go!
