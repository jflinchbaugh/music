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

(defn flatten1
  "Takes a map and returns a seq of all the key val pairs:
      (flatten1 {:a 1 :b 2 :c 3}) ;=> (:b 2 :c 3 :a 1)"
  [m]
  (reduce (fn [r [arg val]] (cons arg (cons val r))) [] m))

(defn live-sequencer
  ([curr-t sep-t live-patterns] (live-sequencer curr-t sep-t live-patterns 0))
  ([curr-t sep-t live-patterns beat]
   (doseq [[sound pattern] @live-patterns
           :let [v (nth pattern (mod beat (count pattern)))
                 v (cond
                     (= 1 v)
                     []

                     (map? v)
                     (flatten1 v)

                     :else
                     nil)]
           :when v]
     (at curr-t (apply sound v)))
   (let [new-t (+ curr-t sep-t)]
     (apply-by new-t #'live-sequencer [new-t sep-t live-patterns (inc beat)]))))

(comment

  (live-sequencer (+ 200 (now)) 200 live-pats)

  (stop)

  (do
    (swap! live-pats assoc subby [0 0 0 0 0 0 0 0])
    (swap! live-pats assoc snare [0 0 0 0 0 0 0 0])
    (swap! live-pats assoc wop   [0 0 0 0 0 0 0 0])
  )

  (swap! live-pats assoc subby [1 1 0 1 0 0 1 1])
  (swap! live-pats assoc snare [1 1 0 0 0 1 0 0])
  (swap! live-pats assoc wop   [1 0 0 0 0 0 0 0])

  (swap! live-pats assoc subby [1 0 0 0 1 0 0 0 1 0 0 1 0 1 0 0])
  (swap! live-pats assoc snare [1 1 0 1 0 1 0 1])

  (swap! live-pats assoc wop   [1 0 0 1 1 0 0 1])

  (swap! live-pats assoc p/sampled-piano  [0 0 0 0 0 0 0])
  (swap! live-pats dissoc p/sampled-piano)

  (def a {:rate 0.5})
  (def b {:rate 3})
  (def c {:rate 10})

  (swap! live-pats assoc subby [1 1 0 b 0 1 a c])
  (swap! live-pats assoc snare [1 1 c c 1 a b c])
  (swap! live-pats assoc wop   [c c 1 0 0 0 a c])

  )
