(ns music.dnb
  (:require [overtone.core :refer :all]
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
(def subby #((freesound 25649) 1 0 1 0 0.5))

(def kick (partial d/kick 50 0.5 1 0.25))

(comment
  (reset! pat [])

  (reset! pat (concat
                ;; drums
                [
                 [0 kick] ;; (midi->hz (note :c2)) 1]
                 [1 d/snare]
                 [2.5 kick] ;; (midi->hz (note :c2)) 10]
                 [3 d/snare]
                 ]

                ;;hats
                (map (fn [i] [i d/closed-hat2]) (range 0 4 0.5))

                ;; shuffle
                #_[[1.125 d/closed-hat2]
                   [1.25 d/closed-hat2]
                   ]))

  (beat metro (metro) 4 pat)

  (stop)

  nil)
