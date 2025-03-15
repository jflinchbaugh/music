(ns music.melody
  (:require [overtone.core :refer :all]
            [overtone.inst.drum :as d]))

(definst ding [freq 400 amp 0.5 attack 0.02 sustain 0.5 release 0.01]
  (* amp (env-gen (perc attack sustain 1 -1) :action FREE) (pan2 (square freq))))

(definst whistle [freq 400 amp 0.5 attack 0.01 sustain 0.5 release 0.01]
  (* amp
    (env-gen (lin attack (- sustain attack release) release) :action FREE)
    (pan2 (sin-osc freq))))

(definst square-buzz [freq 400 amp 0.5 attack 0.01 sustain 0.5 release 0.1]
  (*
    (env-gen
      (adsr-ng attack 0.1 (- sustain attack release) release amp (* 0.75 amp) -4 0)
      :action FREE)
    (pan2 (square freq))))

(definst saw-buzz [freq 400 amp 0.5 attack 0.01 sustain 0.5 release 0.01]
  (* amp
    (env-gen (lin attack (- sustain attack release) release) :action FREE)
    (pan2 (saw freq))))

(comment
  (let [scale-name :minor
        instrument whistle
        bpm 120
        beat-length 1/4
        scale-root :c3
        note-stretch 0.5
        metro (metronome bpm)
        note-duration (* note-stretch 1/1000 beat-length (- (metro 1) (metro 0)))
        progression-1 (take 4 (shuffle (flatten (repeat 4 (cons 0 (chord scale-root scale-name))))))
        progression-2 (take 4 (shuffle (flatten (repeat 4 (cons 0 (chord scale-root scale-name))))))
        progression-3 (reverse progression-1)
        progression-4 (reverse progression-2)
        stream (map #(-> [%1 %2])
                 (concat
                   (flatten (repeat 4 progression-1))
                   (flatten (repeat 4 progression-2))
                   (flatten (repeat 4 progression-1))
                   (flatten (repeat 4 progression-2))
                   (flatten (repeat 4 progression-3))
                   (flatten (repeat 4 progression-4))
                   (flatten (repeat 4 progression-3))
                   (flatten (repeat 4 progression-4))
                   )
                 (map #(* % beat-length) (range)))
        _ (prn progression-1 progression-2 progression-3 progression-4)
        ]
    (doseq [[n b] stream]
      #_(at (metro b) (d/snare))
      (when (zero? (mod b 4))
        (at
          (metro (inc b))
          (d/kick)))
      (when (zero? (mod b 4))
        (at
          (metro (+ 2 b))
          (d/snare)))
      (when-not (zero? n)
        (at
          (metro (inc b))
          (instrument (midi->hz n) (/ 1 note-stretch) 0.1 note-duration 0.01)))))

  (stop)

  (chord :g4 :minor7)

  )
