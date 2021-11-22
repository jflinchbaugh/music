(ns music.core
  (:require [overtone.core :refer :all]
            [clojure.java.shell :refer [sh]]))

(defonce external-server
  (do
    (println "Connecting to supercollider")
    (connect-external-server)))

(defn looper
  ([delay f]
   (looper (now) delay f))
  ([t delay f]
   (at t (f))
   (let [next-t (+ t delay)]
     (apply-by next-t #'looper [next-t delay f]))))

(defonce active-event-handlers* (atom '()))

(defn start-event-handler [player & args]
  (swap! active-event-handlers* conj (apply player args)))

(defn stop-event-handlers [event-handler-ids]
  "Given a list of event-handler-ids returned by inst-player, remove
  all event handlers."
  (doseq [id event-handler-ids]
    (remove-event-handler id)))

(defn stop-active-event-handlers []
  (doseq [p @active-event-handlers*]
    (stop-event-handlers p)
    (prn p)
    (swap! active-event-handlers* (fn [coll item] (remove #{item} coll)) p)))
