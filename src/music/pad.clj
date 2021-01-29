(ns music.pad
  (:require [overtone.core :refer :all]))

(def rcv (midi-find-connected-receiver "SmartPAD"))

(def colors {:black 0
             :white 8
             :yellow 24
             :cyan 40
             :purple 56
             :blue 72
             :green 88
             :red 104})

(defn light-off [pad]
  (let [note (if (coll? pad) (+ (* 16 (first pad)) (second pad)) pad)]
    (midi-note-off rcv note)))

(defn light-on
  ([pad color]
   (let [note (if (coll? pad) (+ (* 16 (first pad)) (second pad)) pad)
         vel (get colors color color)]
     (do
       (light-off pad)
       (midi-note-on rcv note vel))))
  ([pad color dur]
   (let [note (if (coll? pad) (+ (* 16 (first pad)) (second pad)) pad)
         vel (get colors color color)]
     (do
       (light-off pad)
       (midi-note rcv note vel dur)))))

(comment
  (let [cc (cycle (rest (keys colors)))]
    (doseq [r (range 8)
            c (range 8)]
      (light-on [r c] (nth cc (+ r c)) 100)))

  (light-on [1 0] :white 100)

  (midi-connected-receivers)

  .)
