(ns music.arpeggiator
  (:require [overtone.core :refer :all]))

(def metro (metronome 140))

(definst sine-blip [freq 400 attack 0.025 release 1.0 amp 0.4]
  (let [main-level 0.5
        saw-level 0.1
        saw-release-ratio 0.3
        main-env (env-gen (perc attack release) :action FREE)
        saw-env (env-gen (perc attack (* saw-release-ratio release)) :action FREE)]
    (* amp
       (+
        (* main-env main-level (sin-osc freq))
        (* saw-env saw-level (sin-osc (* 2 freq)))))))

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
               (take length))
        _ (prn notes)]
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

(defn play [metro beat root scale-name notes-per-beat snd]
  (player
    metro
    beat
    (/ 1 notes-per-beat)
    (map midi->hz
      (phrase-generator
        (partial random-scale-notes root scale-name 4 8)
        (* 32 notes-per-beat)))
    snd))

(comment
  (let [scale-name :minor]
    (play metro (+ 0 (metro)) :c4 scale-name 1 #'sine-blip)
    (play metro (+ 32.5 (metro)) :c4 scale-name 1 #'sine-blip)
    (play metro (+ 64.25 (metro)) :c4 scale-name 1 #'sine-blip)
    (play metro (+ 64.75 (metro)) :c4 scale-name 1 #'sine-blip)

    (play metro (+ 32 (metro)) :c2 scale-name 1/4 #(sine-blip :freq % :release 2.0))
    (play metro (+ 96 (metro)) :c3 scale-name 2 #'sine-blip)
    )

  (stop)

  nil)
