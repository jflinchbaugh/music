(ns music.melody
  (:require [overtone.live :refer :all]
            [overtone.synth.stringed :refer :all]))

(definst foo [freq 400] (* 0.3 (saw freq)))

(definst ding [freq 400 amp 0.5 sustain 0.5]
  (* amp (env-gen (perc 0.02 sustain 1 -1) :action FREE) (pan2 (sin-osc freq))))

(definst whistle [freq 400 amp 0.5 attack 0.01 sustain 0.5 release 0.01]
  (* amp (env-gen (lin attack (- sustain attack release) release) :action FREE) (pan2 (sin-osc freq))))

(comment
  (let [start-time (now)
        beat-time 300
        chord-name :major
        progression-1 (take 3 (shuffle (scale :c4 chord-name)))
        progression-2 (take 3 (shuffle (scale :e4 chord-name)))
        stream (map #(-> [%1 %2])
                 (concat
                   progression-1
                   [0]
                   progression-1
                   [0]
                   progression-2
                   [0]
                   progression-2
                   [0]
                   progression-1
                   [0]
                   progression-1
                   [0]
                   progression-2
                   [0]
                   progression-2
                   [0])
                 (range))
        ]
    (prn (map first stream))
    (doseq [[n t] stream]
      (at (+ start-time (* beat-time t)) (whistle (midi->hz n) 0.5 0.01 0.3 0.01))))

  )
