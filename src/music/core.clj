(ns music.core
  (:require [overtone.core :refer :all]
            [clojure.java.shell :refer [sh]]))

(do
  #_(println "Starting an external supercollider")
  #_(future (sh "supercollider.sh"))
  (println "Connecting to supercollider")
  (connect-external-server))

(defn looper
  ([delay f]
   (looper (now) delay f))
  ([t delay f]
   (at t (f))
   (let [next-t (+ t delay)]
     (apply-by next-t #'looper [next-t delay f]))))
