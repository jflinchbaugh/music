(ns music.core
  (:require [overtone.live :refer :all]
            [overtone.inst.drum :as d]
            [overtone.inst.sampled-piano :as sp]
            [overtone.inst.piano :as p]
            [overtone.inst.sampled-flute :as f]))

(defn looper
  ([delay f]
   (looper (now) delay f))
  ([t delay f]
   (at t (f))
   (let [next-t (+ t delay)]
     (apply-by next-t #'looper [next-t delay f]))))
