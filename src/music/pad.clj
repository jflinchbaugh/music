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
  (first (shuffle (rest (keys colors)))))

(defonce active-players* (atom '()))

(defn start-player [player & args]
  (swap! active-players* conj (apply player args)))

(defn stop-inst-player [event-handler-ids]
  "Given a list of event-handler-ids returned by inst-player, remove
  all event handlers."
  (doseq [id event-handler-ids]
    (remove-event-handler id)))

(defn stop-active-players []
  (doseq [p @active-players*]
    (stop-inst-player p)
    (prn p)
    (swap! active-players* (fn [coll item] (remove #{item} coll)) p)))

(defn pad-handler []
  (let [id-on-handler (keyword (gensym "on-handler"))
        id-off-handler (keyword (gensym "off-handler"))
        id-ctl-handler (keyword (gensym "ctl-handler"))]
    (on-event [:midi :note-on]
      (fn [{:keys [note velocity velocity-f channel] :as e}]
        (prn " on" channel note (note->coord note) velocity))
      id-on-handler)
    (on-event [:midi :note-off]
      (fn [{:keys [note velocity velocity-f channel] :as e}]
        (prn "off" channel note (note->coord note) velocity))
      id-off-handler)
    (on-event [:midi :control-change]
      (fn [{:keys [note velocity velocity-f channel] :as e}]
        (prn "ctl" channel note velocity))
      id-ctl-handler)
    [id-on-handler id-off-handler id-ctl-handler]))

(comment
  (light-grid (fn [r c] (nth (cycle (rest (keys colors))) (+ (* 2 c) (* 2 r)))))

  (light-grid random-lights)

  (light-on [0 2] :white)

  (midi-connected-receivers)

  (apply coord->note (note->coord 10))

  (start-player pad-handler)

  (stop-active-players)

  .)
