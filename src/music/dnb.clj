(ns music.dnb
  (:require [overtone.live :refer :all]
            [overtone.inst.drum :as d]))

  ;; https://www.youtube.com/watch?v=GdA63c8lvNw

  (def metro (metronome 174))

  (defn beat [m beat-num pat-length pat]
    (doseq [[b i & p] @pat]
      (at (m (+ b beat-num)) (apply i p)))
    (apply-by
     (m (+ pat-length beat-num))
     beat m (+ pat-length beat-num) pat-length pat []))

  (def pat (atom []))

(comment
  (reset! pat [])

  (reset! pat (concat
                ;; base drums
                [[0 d/kick] ;; (midi->hz (note :c2)) 1]
                 [1 d/snare]
                 [2.5 d/kick] ;; (midi->hz (note :c2)) 10]
                 [3 d/snare 1 1]]

                ;;hats
                [[0 d/closed-hat2]
                 [0.5 d/closed-hat2]
                 [1 d/closed-hat2]
                 [1.5 d/closed-hat2]
                 [2 d/closed-hat2]
                 [2.5 d/closed-hat2]
                 [3 d/closed-hat2]
                 [3.5 d/closed-hat2]]

                ;; shuffle
                [[1.125 d/closed-hat2]
                   [1.25 d/closed-hat2]
                   ]))

  (beat metro (metro) 4 pat)

  (stop)

  nil)
