(ns music.core
  (:require [overtone.live :refer :all]
            [overtone.inst.drum :as d]
            [overtone.inst.sampled-piano :as p]
            [overtone.inst.sampled-flute :as f]))

(def ring-hat (freesound 12912))
(def snare (freesound 26903))
(def click (freesound 406))
(def wop (freesound 85291))
(def subby (freesound 25649))

(def pats {subby [0 0 0 0 0 0 0 0]
           snare [0 0 0 0 0 0 0 0]
           wop   [0 0 0 0 0 0 0 0]})

(def live-pats (atom pats))

(defn live-sequencer
  ([curr-t sep-t live-patterns] (live-sequencer curr-t sep-t live-patterns 0))
  ([curr-t sep-t live-patterns beat]
   (doseq [[sound pattern] @live-patterns
           :when (= 1 (nth pattern (mod beat (count pattern))))]
     (at curr-t (sound)))
   (let [new-t (+ curr-t sep-t)]
     (apply-by new-t #'live-sequencer [new-t sep-t live-patterns (inc beat)]))))

(comment

  (live-sequencer (+ 200 (now)) 200 live-pats)

  (swap! live-pats assoc subby [1 1 0 1 0 0 1 1])

  (swap! live-pats assoc snare [1 1 0 0 0 1 0 0])

  (swap! live-pats assoc wop   [1 0 1 0 0 0 1 1])

  (stop)

  )
