(ns music.arpeggiator
  (:require [overtone.core :refer :all]))

(def metro (metronome 140))

(definst sine-blip [freq 400 attack 0.025 release 1.0 amp 0.4]
  (let [level-1 0.5
        level-2 0.1
        level-3 0.05
        level-4 0.05
        release-ratio-2 0.3
        release-ratio-3 1.0
        release-ratio-4 1.0
        env-1 (env-gen (perc attack release) :action FREE)
        env-2 (env-gen (perc attack (* release-ratio-2 release)) :action FREE)
        env-3 (env-gen (perc attack (* release-ratio-3 release)) :action FREE)
        env-4 (env-gen (perc attack (* release-ratio-4 release)) :action FREE)]
    (* amp
       (+
        (* env-1 level-1 (sin-osc freq))
        (* env-2 level-2 (sin-osc (* 2 freq)))
        (* env-2 level-3 (sin-osc (* 3 freq)))
        (* env-4 level-4 (sin-osc (* 4 freq)))))))

(defn player [m num step r sound]
  (let [n (first r)]
    (when n
      (at (m num)
          (sound n))
      (apply-by (m (+ num step)) #'player [m (+ num step) step (rest r) sound]))))

(defn random-scale-notes [root scale-name repeat-notes length]
  (let [notes (->>
               (scale root scale-name)
               (repeat repeat-notes)
               flatten
               shuffle
               cycle
               (take length))]
    notes))

(defn random-chord-notes [root chord-name repeat-notes length]
  (let [notes (->>
                (chord root chord-name)
                (repeat repeat-notes)
                flatten
                shuffle
                cycle
                (take length))]
    notes))

(defn cycle-notes [notes-fn notes-per-phrase]
  (let [notes (->>
               (notes-fn)
               cycle
               (take notes-per-phrase))]
    notes))

(defn phrase-generator [notes-fn notes-per-phrase]
  (->>
   (repeatedly (partial cycle-notes notes-fn notes-per-phrase))
   flatten))

(defn play [metro beat root scale-name notes-per-beat length snd]
  (player
   metro
   beat
   (/ 1 notes-per-beat)
   (->>
    (phrase-generator
     (partial random-chord-notes root scale-name 8 8)
     (* 32 notes-per-beat))
    (map midi->hz)
    (take (* notes-per-beat length)))
   snd))

(defn bars [n]
  (* 32 n))

(metro 1)

(comment
  (let [scale-name :major
        length 8
        note-dur (/ 60 (metro-bpm metro) 0.9)]
    (play metro (+ (bars 0) (metro))
      :c4 scale-name
      1 (bars (+ length 0))
      #(sine-blip :freq % :release note-dur))

    (play metro (+ (bars 1) (metro))
      :c2 scale-name
      1/4 (bars (- length 1))
      #(sine-blip :freq % :release (* 4 note-dur) :amp 0.6))

    (play metro (+ (bars 2) 0.5 (metro))
      :c4 scale-name
      1 (bars (- length 2 1))
      #(sine-blip :freq % :release note-dur))

    (play metro (+ (bars 4) (metro))
      :c3 scale-name
      2 (bars (- length 4 2))
      #(sine-blip :freq % :release note-dur))

    (for [b (range 3 (- length 2) 2)]
      (do
        (play metro (+ (bars b) 0.25 (metro))
          :c4 scale-name
          1 (bars 1)
          #(sine-blip :freq % :release note-dur))
        (play metro (+ (bars b) 0.75 (metro))
          :c4 scale-name
          1 (bars 1)
          #(sine-blip :freq % :release note-dur)))))

  (stop)

  (let [root :a3
        scale-name :major
        notes-per-beat 1
        length 256]
    (->>
     (phrase-generator
      (partial random-chord-notes root scale-name 4 8)
      (* 32 notes-per-beat))
     (take (* notes-per-beat length))))

  (scale :a3 :major);; => (57 59 61 62 64 66 68 69)

  (scale :a3 :minor);; => (57 59 60 62 64 65 67 69)

  (scale :a3 :phrygian);; => (57 58 60 62 64 65 67 69)

  nil)
