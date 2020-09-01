(ns music.dnb
  (:require [overtone.live :refer :all]
            [overtone.inst.drum :as d]))

;; stranjah minimal dnb
;; https://www.youtube.com/watch?v=GdA63c8lvNw

(def metro (metronome 174))

(defn beat [m beat-num pat-length pat]
  (doseq [[b i & p] @pat]
    (at (m (+ b beat-num)) (apply i p)))
  (apply-by
    (m (+ pat-length beat-num))
    beat m (+ pat-length beat-num) pat-length pat []))

(def pat (atom []))

(def ring-hat (freesound 12912))
(def snare (freesound 26903))
(def click (freesound 406))
(def wop (freesound 85291))
(def subby (freesound 25649))

(comment
  (reset! pat [])

  (snare)

  (freesound)
  (reset! pat (concat
                ;; base drums
                [
                 [0 d/kick] ;; (midi->hz (note :c2)) 1]
                 [1 d/snare]
                 [2.5 d/kick] ;; (midi->hz (note :c2)) 10]
                 [3 d/snare]

                 ]

                ;;hats
                (map (fn [i] [i d/closed-hat2]) (range 0 4 0.5))

                ;; shuffle
                [[1.125 d/closed-hat2]
                   [1.25 d/closed-hat2]
                   ]))

  (beat metro (metro) 4 pat)

  (metro-bpm metro 160)

  (stop)

  (map (fn [i] [i d/closed-hat2]) (range 0 4 0.5))

  (demo (+ (env-gen (env-adsr 0.01 1 1 1)) (lpf (+ (sin-osc-fb [50 50] 2)) 100)))

  nil)
