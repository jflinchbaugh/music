(ns music.keyboard
  (:require [overtone.core :refer :all]
            [music.core :refer :all]
            [overtone.inst.drum :as d]
            [clojure.pprint :refer [pprint]]
            [music.pad :refer :all]))

; overtone.studio.midi now includes the beginnings of a higher level midi interface
; that helps improve on this.  By default Overtone will detect and listen to all
; midi input devices.  These midi messages are then sent as events, which can be received
; with the Overtone event system.

; For example, print out all incoming note-on messages:

;(on-event [:midi :note-on] (fn [{note :note velocity :velocity}]
;                             (println "Note: " note ", Velocity: " velocity))
;          ::note-printer)


;(remove-event-handler ::note-printer)

; Other available midi events are:
; * :channel-pressure
; * :control-change
; * :note-off
; * :note-on
; * :pitch-bend
; * :poly-pressure
; * :program-change

; In order to play instruments that continue playing until a key is released,
; we need to keep track of each active synth instance once triggered by :note-on,
; and then send a control message to either kill it or close the gate on an
; envelope so the release starts.  This is what overtone.studio.midi/midi-poly-player
; does for you.  All it requires is that you have exposed an envelope gate as a synth
; parameter called "gate".

(definst poly-ding
  [note 60 amp 1 gate 1]
  (let [freq (midicps note)
        snd  (sin-osc freq)
        env  (env-gen (adsr 0.001 0.1 0.6 0.3) gate :action FREE)]
    (* amp env snd)))

; Create a polyphonic midi player:
;(def ding-player (midi-poly-player poly-ding))

; and stop it:
;(midi-player-stop ding-player)

; Below is a more sophisticated example that demonstrates how to
; control :gate and :sustain parameters, and pitch bend of an instrument based on MIDI
; events. inst-player can be used to handle input from a MIDI keyboard
; and its sustain pedal and pitch bend controller.

; This function transforms pitch-bend midi signal of 14 bits to a
; float between -1.0 and 1.0 where -1.0 means lowest bend,
; 0.0 means no bend, and 1.0 means highest bend.
; See https://www.midikits.net/midi_analyser/pitch_bend.htm for reference.
(defn midi-pitch-bend-to-float [data1 data2]
  (let [; midi two bytes to a number between 0 and 16383,
        ; 0 means lowest bend, 8192 means no bend, 16383 means highest bend
        unsigned (bit-or
                  (-> data2 (bit-flip 6) (bit-shift-left 7))
                  data1)
          ; to a number between -8192 and 8191
        signed (- unsigned 8192)
          ; correcting for having less values (8191) for pitch increase than decrease
        divisor (if (< signed 0) 8192 8191)
        ret (float (/ signed divisor))]
    ret))

(defn inst-player [inst]
  "Handle incoming midi events by playing notes on inst, updating
  the :gate and :sustain parameters of inst based on MIDI
  note-on/note-off and sustain pedal (control-change 64) events
  respectively."
  (let [notes* (atom {; Notes are active if the key is still being
                      ; pressed.
                      :active {}
                      ; Notes are finished if the key is no longer
                      ; being pressed (but it may still be generating
                      ; sound).
                      :finished {}})
        sustain* (atom 0)
        pitch-bend* (atom 0)
        bend-range* (atom 1.0)
        bottom-amp* (atom 0)
        release* (atom 0.5)
        attack* (atom 0.001)
        o1v* (atom 0.0)
        o2v* (atom 0.0)
        o3v* (atom 0.0)

        on-id (keyword (gensym "on-handler"))
        off-id (keyword (gensym "off-handler"))
        cc-id (keyword (gensym "cc-handler"))
        pitch-bend-id (keyword (gensym "pitch-bend-handler"))

        knob1 (fn [v-f] (prn "knob1" v-f) (reset! o1v* v-f) (ctl inst :o1 v-f))
        knob2 (fn [v-f] (prn "knob2" v-f) (reset! o2v* v-f) (ctl inst :o2 v-f))
        knob3 (fn [v-f] (prn "knob3" v-f) (reset! o3v* v-f) (ctl inst :o3 v-f))]

    ; Handle note-on MIDI events.


    (on-event [:midi :note-on]
              (fn [{:keys [note velocity-f]}]
                (let [amp-range (- 1.0 @bottom-amp*)
                      amp (+ @bottom-amp* (* amp-range velocity-f))]
                  ; Ignore the event if this note is already active.
                  (when (not (contains? (:active @notes*) note))
                    (let [sound (get-in @notes* [:finished note])]
                      ; If there is a "finished" version of this note,
                      ; set gate and sustain to zero to prevent
                      ; overlapping with the new note.
                      (when (and sound (node-active? sound))
                        (node-control sound [:gate 0 :sustain 0]))
                      ; Create a new note and set it to "active".
                      (swap! notes* assoc-in [:active note]
                             (inst
                              :note note
                              :amp amp
                              :attack @attack*
                              :release @release*
                              :sustain @sustain*
                              :pitch-bend @pitch-bend*
                              :o1 @o1v*
                              :o2 @o2v*
                              :o3 @o3v*))))
                  (prn "note on" note amp @attack* @release*)))
              on-id)

    ; Handle note-off MIDI events.
    (on-event [:midi :note-off]
              (fn [{:keys [note velocity-f]}]
                (let [velocity velocity-f]
                  (when-let [sound (get-in @notes* [:active note])]
                    ; Set the note's gate to 0, and move it
                    ; from "active" to "finished".
                    (with-inactive-node-modification-error :silent
                      (node-control sound [:gate 0 :after-touch velocity]))
                    (swap! notes* update-in [:active] dissoc note)
                    (swap! notes* assoc-in [:finished note] sound))
                  (prn "note off" note velocity)))
              off-id)

    ; Handle control-change MIDI events.
    (on-event [:midi :control-change]
              (fn [{:keys [note data2 data2-f]}]
                (case note
                  ; Note 64 = MIDI sustain pedal control-change
                  64 (do (prn "pedal" data2))
                  106 (let [sustain? (>= data2 64)
                            sustain (if sustain? 1 0)]
                       ; Update the sustain atom, and update the
                       ; sustain of all active notes.
                        (reset! sustain* sustain)
                        (if sustain?
                          (doseq [sound (:active @notes*)]
                            (ctl sound :sustain sustain))
                          (ctl inst :sustain sustain)))
                  7 (do
                      (reset! bottom-amp* data2-f)
                      (prn "bottom-amp" data2-f))
                  10 (prn "data" data2-f)
                  21 (knob1 data2-f)
                  #_(let [attack (* 1.0 data2-f)]
                      (reset! attack* attack)
                      (prn "knob1/attack" data2-f))
                  22 (knob2 data2-f)
                  23 (knob3 data2-f)
                  #_(let [release (* 10 data2-f)]
                      (reset! release* release)
                      (prn "knob3/release" release))
                  1 (let [r (* 10 data2-f)]
                      (reset! bend-range* r)
                      (prn "bend range" r))
                  105 (do (stop) (prn "play/stop"))
                  107 (do (stop) (prn "rec/stop"))
                  (prn "cc" note data2)))
              cc-id)

    ; Handle pitch-bend MIDI events
    (on-event [:midi :pitch-bend]
              (fn [{:keys [data1 data2]}]
                (let [pitch-f (midi-pitch-bend-to-float data1 data2)]
                  ; Update pitch bend, even if no nodes are active.
                  (reset! pitch-bend* (* @bend-range* pitch-f))
                  ; Ignore bending pitch if no nodes are active
                  (when (not (empty? (:active @notes*)))
                    (ctl inst :pitch-bend @pitch-bend*))
                  (prn "pitch bend" @pitch-bend*)))
              pitch-bend-id)

    ; Return the ids of the event handlers so they can be stopped
    ; later.
    [on-id off-id cc-id pitch-bend-id]))

(comment 
  (defn stop-inst-player [event-handler-ids]
    "Given a list of event-handler-ids returned by inst-player, remove
  all event handlers."
    (doseq [id event-handler-ids]
      (remove-event-handler id))))

; Create an instrument with a sustain parameter.
(definst sustain-ding
  [note 60 amp 1 gate 1 sustain 0 pitch-bend 0 release 0.5 attack 0.001]
  (let [freq (midicps (+ note pitch-bend))
        snd  (sin-osc freq)
        env  (env-gen (adsr attack 0.1 0.6 release) (or gate sustain) :action FREE)]
    (* amp env snd)))

; Create an instrument with a sustain parameter.
(definst sustain-flute
  [note 60 amp 1 gate 1 sustain 0 pitch-bend 0 release 0.5 attack 0.001]
  (let [freq (midicps (+ note pitch-bend))
        snd  (sin-osc freq)
        snd2 (* 0 (sin-osc (* 2 freq)))
        snd3 (* 1/4 (sin-osc (* 3 freq)))
        env  (env-gen (adsr attack 0.1 0.6 release) (or gate sustain) :action FREE)]
    (* amp env (/ (+ snd snd2 snd3) 5/4))))

(definst sustain-saw
  [note 60 amp 1 gate 1 sustain 0 pitch-bend 0 release 0.5 attack 0.001]
  (let [freq (midicps (+ note pitch-bend))
        snd  (saw freq)
        env  (env-gen (adsr attack 0.1 0.6 release) (or gate sustain) :action FREE)]
    (* amp env snd)))

(definst sustain-harmonic
  [note 60
   amp 1
   gate 1
   sustain 0
   pitch-bend 0
   release 0.5
   attack 0.001
   o1 0.0
   o2 0.0
   o3 0.0]
  (let [freq (midicps (+ note pitch-bend))
        snd  (sin-osc freq)
        snd1 (* o1 (sin-osc (* 3 freq)))
        snd2 (* o2 (sin-osc (* 5 freq)))
        snd3 (* o3 (sin-osc (* 7 freq)))
        env (env-gen  (adsr attack 0.1 1.0 release) (or gate sustain) :action FREE)]
    (* amp env (+ snd snd1 snd2 snd3) (/ 1 (+ 1 o1 o2 o3)))))

(defn- prn-event [e]
  (println "=================")
  (->
   e
   (dissoc :msg :velocity :velocity-f :note :device)
   pprint))

(defn log-player []
  (let [prn #(prn-event %)
        on-id (keyword (gensym "on-handler"))
        off-id (keyword (gensym "off-handler"))
        cc-id (keyword (gensym "cc-handler"))
        pitch-id (keyword (gensym "pitch-handler"))
        pp-id (keyword (gensym "pp-handler"))
        cp-id (keyword (gensym "cp-handler"))
        pc-id (keyword (gensym "pc-handler"))]
    (on-event [:midi :note-on] prn on-id)
    (on-event [:midi :note-off] prn off-id)
    (on-event [:midi :control-change] prn cc-id)
    (on-event [:midi :pitch-bend] prn pitch-id)
    (on-event [:midi :poly-pressure] prn pp-id)
    (on-event [:midi :channel-pressure] prn cp-id)
    (on-event [:midi :program-change] prn pc-id)

    ; Return the ids of the event handlers so they can be stopped
    ; later.
    [on-id off-id cc-id pitch-id pp-id cp-id pc-id]))

(def all-samples [
  "clap-808"
  "clap-analog"
  "clap-crushed"
  "clap-fat"
  "clap-slapper"
  "clap-tape"
  "cowbell-808"
  "crash-808"
  "crash-acoustic"
  "crash-noise"
  "crash-tape"
  "hihat-808"
  "hihat-acoustic01"
  "hihat-acoustic02"
  "hihat-analog"
  "hihat-digital"
  "hihat-dist01"
  "hihat-dist02"
  "hihat-electro"
  "hihat-plain"
  "hihat-reso"
  "hihat-ring"
  "kick-808"
  "kick-acoustic01"
  "kick-acoustic02"
  "kick-big"
  "kick-classic"
  "kick-cultivator"
  "kick-deep"
  "kick-dry"
  "kick-electro01"
  "kick-electro02"
  "kick-floppy"
  "kick-gritty"
  "kick-heavy"
  "kick-newwave"
  "kick-oldschool"
  "kick-plain"
  "kick-slapback"
  "kick-softy"
  "kick-stomp"
  "kick-tape"
  "kick-thump"
  "kick-tight"
  "kick-tron"
  "kick-vinyl01"
  "kick-vinyl02"
  "kick-zapper"
  "openhat-808"
  "openhat-acoustic01"
  "openhat-analog"
  "openhat-slick"
  "openhat-tight"
  "perc-808"
  "perc-chirpy"
  "perc-hollow"
  "perc-laser"
  "perc-metal"
  "perc-nasty"
  "perc-short"
  "perc-tambo"
  "perc-tribal"
  "perc-weirdo"
  "ride-acoustic01"
  "ride-acoustic02"
  "shaker-analog"
  "shaker-shuffle"
  "shaker-suckup"
  "snare-808"
  "snare-acoustic01"
  "snare-acoustic02"
  "snare-analog"
  "snare-big"
  "snare-block"
  "snare-brute"
  "snare-dist01"
  "snare-dist02"
  "snare-dist03"
  "snare-electro"
  "snare-lofi01"
  "snare-lofi02"
  "snare-modular"
  "snare-noise"
  "snare-pinch"
  "snare-punch"
  "snare-smasher"
  "snare-sumo"
  "snare-tape"
  "snare-vinyl01"
  "snare-vinyl02"
  "tom-808"
  "tom-acoustic01"
  "tom-acoustic02"
  "tom-analog"
  "tom-chiptune"
  "tom-fm"
  "tom-lofi"
  "tom-rototom"
  "tom-short"])

(def sampler-grid
  [["clap-808"
    "clap-analog"
    "clap-crushed"
    "clap-fat"
    "clap-slapper"
    "clap-tape"
    "cowbell-808"
    "crash-808"]

   ["crash-acoustic"
    "crash-noise"
    "crash-tape"
    "hihat-808"
    "hihat-acoustic01"
    "hihat-acoustic02"
    "hihat-analog"
    "hihat-digital"]

   ["hihat-dist01"
    "hihat-dist02"
    "hihat-electro"
    "hihat-plain"
    "hihat-reso"
    "hihat-ring"
    "kick-808"
    "kick-acoustic01"]

   ["kick-acoustic02"
    "kick-big"
    "kick-classic"
    "kick-cultivator"
    "kick-deep"
    "kick-dry"
    "kick-electro01"
    "kick-electro02"]

   ["kick-floppy"
    "kick-gritty"
    "kick-heavy"
    "kick-newwave"
    "kick-oldschool"
    "kick-plain"
    "kick-slapback"
    "kick-softy"]

   ["kick-stomp"
    "kick-tape"
    "kick-thump"
    "kick-tight"
    "kick-tron"
    "kick-vinyl01"
    "kick-vinyl02"
    "kick-zapper"]

   ["openhat-808"
    "openhat-acoustic01"
    "openhat-analog"
    "openhat-slick"
    "openhat-tight"
    "perc-808"
    "perc-chirpy"
    "perc-hollow"]

   ["perc-laser"
    "perc-metal"
    "perc-nasty"
    "perc-short"
    "perc-tambo"
    "perc-tribal"
    "perc-weirdo"
    "ride-acoustic01"]])

(defn color-patterns [f-name]
  (condp re-matches f-name
    #"kick-.*" :red
    #"tom-.*" :red
    #"clap-.*" :blue
    #"perc-.*" :green
    #"cowbell-.*" :green
    #"hihat-.*" :cyan
    #"openhat-.*" :cyan
    #"crash-.*" :cyan
    #"ride-.*" :cyan
    #"snare-.*" :purple
    :white))

(defn load-sample-grid [sample-grid]
  (doall
   (mapv
    (fn [row]
      (doall
       (mapv
        (fn [f-name]
          (let [sample-name (str "samples/99sounds/" f-name ".wav")
                sample (sample sample-name)
                instrument (->>
                            sample
                            (play-buf 1)
                            (out [0 1])
                            (synth f-name))
                color (color-patterns f-name)
                _ (prn sample-name instrument)]
            [sample color]))
        row)))
    sample-grid)))

(defsynth my-sample-player
  [sample 0 amp 1]
  (let [snd (play-buf 1 sample :action FREE)]
    (out [0 1] (* amp snd))))

(defn drum-player [quantize sample-name-grid]
  (let [notes* (atom {:active {} :finished {}})
        on-id (keyword (gensym "on-handler"))
        off-id (keyword (gensym "off-handler"))
        cc-id (keyword (gensym "cc-handler"))
        metro (metronome 174)
        control-change (fn [data-1 data-2])
        sample-name-grid-v (mapv vec sample-name-grid)
        sample-grid (load-sample-grid sample-name-grid)
        play-note (fn [n]
                    (let [sample (get-in sample-grid (concat (note->coord n) [0]))]
                      (when sample (at (metro (+ quantize (metro))) (my-sample-player sample)))))]
    (doseq [r (range 8)
            c (range 8)]
      (light-on [r c] (get-in sample-grid [r c 1] :black)))
    (on-event [:midi :note-on]
              (fn [{:keys [note velocity-f]}]
                ; Ignore the event if this note is already active.
                (when (not (contains? (:active @notes*) note))
                  (let [sound (get-in @notes* [:finished note])]
                    ; If there is a "finished" version of this note,
                    ; set gate and sustain to zero to prevent
                    ; overlapping with the new note.
                    (when (and sound (node-active? sound))
                      (node-control sound [:gate 0 :sustain 0]))
                    ; Create a new note and set it to "active".
                    (swap! notes* assoc-in [:active note] sound)))
                (prn (note->coord note) (get-in sample-name-grid-v (note->coord note)))
                (light-on (note->coord note) :black)
                (play-note note))
              on-id)

    ; Handle note-off MIDI events.
    (on-event [:midi :note-off]
              (fn [{:keys [note velocity-f]}]
                (when-let [sound (get-in @notes* [:active note])]
                    ; Set the note's gate to 0, and move it
                    ; from "active" to "finished".
                  (with-inactive-node-modification-error :silent
                    (node-control sound [:gate 0 :after-touch velocity-f]))
                  (swap! notes* update-in [:active] dissoc note)
                  (swap! notes* assoc-in [:finished note] sound))
                (light-on
                 (note->coord note)
                 (get-in sample-grid (concat (note->coord note) [1]) :black)))
              off-id)

    ; Handle control-change MIDI events.
    (on-event [:midi :control-change]
              (fn [{:keys [data-1 data2]}] control-change) cc-id)

    ; Return the ids of the event handlers so they can be stopped
    ; later.
    [on-id off-id cc-id]))

(comment
  (start-event-handler drum-player)

  (start-event-handler log-player)

  (stop-active-event-handlers)

  (stop)

  (volume 1)

  (event [:midi :note-on] :note 100 :velocity-f 0.5)
  (event [:midi :note-off] :note 100 :velocity-f 0)

  (midi-connected-devices)

  (m/midi-ports)

  (-> (m/midi-sinks) first (midi-note-on 64 1.0))

  (m/midi-out)

  (doseq [k (range 128)]
    (midi-note-off (first (midi-connected-receivers)) k))

  (midi-note-off (first (midi-connected-receivers)) 65)

  (midi-note (first (midi-connected-receivers)) 67 1 1000 1)

  (start-event-handler log-player)

  (stop-active-event-handlers)

  (load-sample-grid (take 1 sampler-grid))

  ;; subset
  (start-event-handler
    drum-player
    0
    (take 8
      (partition 8
        (filter
          #(re-matches #".*(808|analog|acoustic|vinyl|tape|perc).*" %)
          all-samples))))

  ;; all
  (start-event-handler
    drum-player 0
    (take 8
      (partition 8 all-samples)))

  (start-event-handler inst-player sustain-saw )

  (stop-active-event-handlers)

  (demo (sin-osc))

  .)

