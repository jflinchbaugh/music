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

(defn coord->note [row col]
  (+ (* 16 row) col))

(defn note->coord [note]
  [(int (/ note 16)) (mod note 16)])

(defn light-off [pad]
  (let [note (if (coll? pad) (apply coord->note pad) pad)]
    (midi-note-off rcv note)))

(defn light-on
  ([pad color]
   (let [note (if (coll? pad) (apply coord->note pad) pad)
         vel (get colors color color)]
     (do
       (light-off pad)
       (midi-note-on rcv note vel))))
  ([pad color dur]
   (let [note (if (coll? pad) (apply coord->note pad) pad)
         vel (get colors color color)]
     (do
       (light-off pad)
       (midi-note rcv note vel dur)))))

(defn light-grid
  "Light up the whole grid with colors determined
  by calling (color-fn row column)."
  [color-fn]
    (doseq [r (range 8)
            c (range 8)]
      (light-on [r c] (color-fn r c))))

(defn random-lights [r c]
  (let [cc (cycle (rest (keys colors)))]
    (-> cc shuffle first)))

(comment
  (light-grid (fn [r c] (first (shuffle (rest (keys colors))))))

  (light-on [0 2] :white)

  (midi-connected-receivers)

  (apply coord->note (note->coord 10))

  .)