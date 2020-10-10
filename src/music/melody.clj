(ns music.melody
  (:require [overtone.live :refer :all]
            [overtone.synth.stringed :refer :all]))

(definst ding [freq 400 amp 0.5 sustain 0.5]
  (* amp (env-gen (perc 0.02 sustain 1 -1) :action FREE) (pan2 (sin-osc freq))))

(definst whistle [freq 400 amp 0.5 attack 0.01 sustain 0.5 release 0.01]
  (* amp (env-gen (lin attack (- sustain attack release) release) :action FREE) (pan2 (square freq))))

(comment
  (let [chord-name :major
        bpm 140
        beat-length 1/2
        metro (metronome bpm)
        note-duration (* 1/1000 beat-length (- (metro 1) (metro 0)))
        progression-1 (take 4 (shuffle (scale :c4 chord-name)))
        progression-2 (take 4 (shuffle (scale :d4 chord-name)))
        stream (map #(-> [%1 %2])
                 (concat
                   (flatten (repeat 4 progression-1))
                   (flatten (repeat 4 progression-2))
                   (flatten (repeat 4 progression-1))
                   (flatten (repeat 4 progression-2))
                   )
                 (map #(* % beat-length) (range)))
        ]
    (prn progression-1 progression-2)
    (doseq [[n b] stream]
      (at (metro (inc b)) (whistle (midi->hz n) 0.75 0.02 note-duration 0.02))))

  )
