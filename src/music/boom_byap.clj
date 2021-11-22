(ns music.boom-byap
  (:require [overtone.core :refer :all]
            [overtone.inst.drum :as d]
            [overtone.inst.synth :as synth]))

(defonce metro (metronome 120))

(def boom d/kick)

(def byap (partial d/closed-hat2 0.5))

(defn bass [n] (synth/bass :freq (midi->hz n) :t 3 :amp 0.3))

(defn bubs [note] (when note (synth/bubbles note)))

(defn lead [note] (when note (synth/cs80lead (midi->hz note) 0.25)))

(defn play-and-kill
  [n m b beat-num length]
  (let [i (at (m (+ b beat-num)) (n))]
    (when i (at (m (+ b length beat-num)) (kill i)))))

(defn play-and-gate
  [n m b beat-num length]
  (let [i (at (m (+ b beat-num)) (n))]
    (when i (at (m (+ b length beat-num)) (node-control i [:gate 0])))))

(defn play [m beat-num]
    ;; boom
  (doseq [b (range 4)]
    (at (m (+ b beat-num)) (boom)))

    ;; extra kick
  (at (m (+ 0.25 beat-num)) (boom))

    ;; byap
  (doseq [b (range 8)]
    (at (m (+ (/ b 2) beat-num)) (byap)))

    ;; chords
  (doseq [[b n] (map list (range 4) (degrees->pitches [:i :i :_ :_] :minor :c2))]
    (when n
      (at (m (+ b beat-num))
          (doseq [cn (take 2 (chord n :minor))] (bass cn)))))

    ;; bubbles
  (doseq [[b n] (map list (range 4) (degrees->pitches [:i :v :i :iii] :minor :c4))]
    (play-and-kill #(bubs n) m b beat-num 0.75))

    ;; lead
  (doseq [[b n] (map list (range 4) (degrees->pitches [:_ :v :_ :iii] :minor :c3))]
    (play-and-gate #(lead n) m b beat-num 0.5))

  (apply-by (m (+ 4 beat-num)) play [m (+ 4 beat-num)]))

(comment

  (play metro (metro))

  (stop)

  nil)
