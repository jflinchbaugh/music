(ns music.keyboard
  (:require [overtone.core :refer :all]
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

(defn stop-inst-player [event-handler-ids]
  "Given a list of event-handler-ids returned by inst-player, remove
  all event handlers."
  (doseq [id event-handler-ids]
    (remove-event-handler id)))

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
        pc-id (keyword (gensym "pc-handler"))
        ]
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
(comment
clap-808.wav
clap-analog.wav
clap-crushed.wav
clap-fat.wav
clap-slapper.wav
clap-tape.wav
cowbell-808.wav
crash-808.wav
crash-acoustic.wav
crash-noise.wav
crash-tape.wav
hihat-808.wav
hihat-acoustic01.wav
hihat-acoustic02.wav
hihat-analog.wav
hihat-digital.wav
hihat-dist01.wav
hihat-dist02.wav
hihat-electro.wav
hihat-plain.wav
hihat-reso.wav
hihat-ring.wav
kick-808.wav
kick-acoustic01.wav
kick-acoustic02.wav
kick-big.wav
kick-classic.wav
kick-cultivator.wav
kick-deep.wav
kick-dry.wav
kick-electro01.wav
kick-electro02.wav
kick-floppy.wav
kick-gritty.wav
kick-heavy.wav
kick-newwave.wav
kick-oldschool.wav
kick-plain.wav
kick-slapback.wav
kick-softy.wav
kick-stomp.wav
kick-tape.wav
kick-thump.wav
kick-tight.wav
kick-tron.wav
kick-vinyl01.wav
kick-vinyl02.wav
kick-zapper.wav
openhat-808.wav
openhat-acoustic01.wav
openhat-analog.wav
openhat-slick.wav
openhat-tight.wav
perc-808.wav
perc-chirpy.wav
perc-hollow.wav
perc-laser.wav
perc-metal.wav
perc-nasty.wav
perc-short.wav
perc-tambo.wav
perc-tribal.wav
perc-weirdo.wav
ride-acoustic01.wav
ride-acoustic02.wav
shaker-analog.wav
shaker-shuffle.wav
shaker-suckup.wav
snare-808.wav
snare-acoustic01.wav
snare-acoustic02.wav
snare-analog.wav
snare-big.wav
snare-block.wav
snare-brute.wav
snare-dist01.wav
snare-dist02.wav
snare-dist03.wav
snare-electro.wav
snare-lofi01.wav
snare-lofi02.wav
snare-modular.wav
snare-noise.wav
snare-pinch.wav
snare-punch.wav
snare-smasher.wav
snare-sumo.wav
snare-tape.wav
snare-vinyl01.wav
snare-vinyl02.wav
tom-808.wav
tom-acoustic01.wav
tom-acoustic02.wav
tom-analog.wav
tom-chiptune.wav
tom-fm.wav
tom-lofi.wav
tom-rototom.wav
tom-short.wav

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
"tom-short"
)

(def sampler-grid

  [
   ["clap-808"
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

(defn load-sample-grid [sample-grid]
  (mapv
    (fn [row]
      (mapv
        (fn [f-name] (sample (str "samples/99sounds/" f-name ".wav")))
        row))
    sample-grid)
  )


(defn drum-player []
  (let [notes* (atom {:active {} :finished {}})
        on-id (keyword (gensym "on-handler"))
        off-id (keyword (gensym "off-handler"))
        cc-id (keyword (gensym "cc-handler"))
        control-change (fn [data-1 data-2] )
        sample-grid (load-sample-grid sampler-grid)
        play-note (fn [n] ((get-in sample-grid (note->coord n))))]

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
                (prn (note->coord note) (get-in sampler-grid (note->coord note)))
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
                  (prn "off" note velocity-f))
              off-id)

    ; Handle control-change MIDI events.
    (on-event [:midi :control-change]
              (fn [{:keys [data-1 data2]}] control-change) cc-id)

    ; Return the ids of the event handlers so they can be stopped
    ; later.
    [on-id off-id cc-id]))

(defonce active-players* (atom '()))

(defn start-player [player]
  (swap! active-players* conj (player)))

(defn stop-active-players []
  (doseq [p @active-players*]
    (stop-inst-player p)
    (prn p)
    (swap! active-players* (fn [coll item] (remove #{item} coll)) p)))

(comment
  (start-player drum-player)

  (start-player log-player)

  (stop-active-players)

  (stop)

  (volume 1)

  (event [:midi :note-on] :note 100 :velocity-f 0.5)
  (event [:midi :note-off] :note 100 :velocity-f 0)

  (midi-connected-devices)

  (m/midi-ports)

  (-> (m/midi-sinks) first (midi-note-on 64 1.0))

  (m/midi-out)


  (m/mi)
  (d/dry-kick)
  (d/closed-hat)
  (d/closed-hat2)
  (d/dub-kick)
  (d/hat3)
  (d/kick)
  (d/snare)

  (d/snare)

  (d/soft-hat)

  (d/bing)

  (note :c4)

  (midi-note-on )

  (metronome 10)

    (let [rcv (first (midi-connected-receivers))]
      (doseq [k (range 128)]
                (do
                 (midi-note-off rcv k)
                   (midi-note-on rcv k k))))

  (doseq [k (range 128)]
    (midi-note-off (first (midi-connected-receivers)) k))

  (midi-note-off (first (midi-connected-receivers)) 65)

  (midi-note (first (midi-connected-receivers)) 67 1 1000 1)


  (start-player log-player)

  (start-player drum-player)

  (stop-active-players)

  (def grid (load-sample-grid sampler-grid))

  ((get-in grid [0 1]))

  (nth grid)

  .)

