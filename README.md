# music

My music explorations in Clojure and Overtone.

## running the workstation

* Ensure `pipewire` is running to serve as our JACK server
* Start supernova and an nrepl: ./supernova.sh
* Ensure that supernova is wired in pipewire to an output
* In Emacs:
** Start emacs
** connect to the running repl
** evaluate all of `music/core.clj`
** you should be connected and ready to go!
