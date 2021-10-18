(ns music.boom-byap
  (:require [overtone.core :refer :all]
            [overtone.inst.drum :as d]
            [overtone.inst.synth :as synth]))

  (defonce metro (metronome 120))

  (def boom d/kick)

  (def byap (partial d/closed-hat2 0.2))

  (metro 1)

  (defn bass [n] (synth/bass :freq (midi->hz n) :t 3 :amp 0.3))

  (defn bubs [note] (synth/bubbles note))

  (defn lead [note] (synth/cs80lead (midi->hz note) 0.25))

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
    (doseq [[b n] (map list (range 4) (degrees->pitches [:i :i :_ :iii] :minor :c2))]
      (when n
        (at (m (+ b beat-num))
          (doseq [cn (take 2 (chord n :minor))] (bass cn)))))

    ;; bubbles
    (doseq [[b n] (map list (range 4) (degrees->pitches [:i :v :i :iii] :minor :c4))]
      (when n
        (let [bubbles (at (m (+ b beat-num)) (bubs n))]
          (at (m (+ b 1 beat-num)) (kill bubbles)))))

    ;; lead
    (doseq [[b n] (map list (range 4) (degrees->pitches [:_ :v :_ :iii] :minor :c4))]
      (when n
        (let [i (at (m (+ b beat-num)) (lead n))]
          (at (m (+ b 0.5 beat-num)) (kill i)))))

    (apply-by (m (+ 4 beat-num)) play [m (+ 4 beat-num)]))


(comment

  (play metro (metro))

  (stop)

  nil)
